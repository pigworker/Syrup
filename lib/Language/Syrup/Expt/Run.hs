------------------------------------------------------------------------------
-----                                                                    -----
-----     Expt: Running Experiments on Syrup Programs                    -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.Syrup.Expt.Run where

import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Control.Monad.Writer (tell, censor)

import Data.Foldable (toList)
import Data.Monoid (Sum(Sum))
import qualified Data.Sequence as Seq

import Language.Syrup.Anf (toANF)
import Language.Syrup.BigArray (singleton, foldMapArr)
import Language.Syrup.Chk (elabPropertyTest)
import Language.Syrup.Cst (costing)
import Language.Syrup.DeMorgan (deMorgan)
import Language.Syrup.DNF (dnf, ttToDef)
import Language.Syrup.Doc (pretty, aLine, prettyBlock, FeedbackStatus(..))
import Language.Syrup.Dot (whiteBoxDef)
import Language.Syrup.Expt
import Language.Syrup.Fdk
import Language.Syrup.Opt
import Language.Syrup.Pretty
import Language.Syrup.Syn (EXPT(..), Name(..), getInputName)
import Language.Syrup.Ty (TypeDecl'(..), Compo(..), InputWire(..), OutputWire(..))
import Language.Syrup.Unelab (runUnelab)
import Language.Syrup.Utils

import Utilities.Lens

import System.Directory (findExecutable)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

experiment :: MonadExperiment s m => EXPT -> m ()
experiment (Tabulate x) = withCompo x $ \ c -> ifSmallEnough c $
  tell $ Seq.singleton $ ATruthTable x $ displayTabulation (tabulate c)
experiment (Simulate x m0 iss) = withCompo x $ \ c -> case runCompo c m0 iss of
  Left fdk -> tell $ Seq.singleton fdk
  Right msg -> anExperiment "Simulation for" [x] msg
experiment (UnitTest x is os) = withCompo x $ \ c ->
  tell $ Seq.singleton $ WhenUnitTesting x is os $ case unitTest c is os of
  Left fdk -> [fdk]
  Right () -> [ASuccessfulUnitTest]
experiment (PropertyTest vars elhs erhs) = do
  g <- use hasLens
  let pe = prettyShow g elhs
  let pf = prettyShow g erhs
  censor (Seq.singleton . WhenPropertyTesting vars pe pf . cleanup . toList) $
    elabPropertyTest (vars, elhs, erhs) >>= \case
      Nothing -> pure ()
      Just (lhs, rhs) -> case bisimReport lhs rhs of
        Report Bisimilar{} -> pure ()
        rep -> tell $ Seq.singleton $ AFailedExperiment $
          report (Name pe, Name pf) rep

  where

    cleanup :: [Feedback] -> [Feedback]
    cleanup fdks = case filter ((`elem` [Error, Internal]) . categorise) fdks of
      [] -> [ASuccessfulPropertyTest]
      err -> err

experiment (Bisimilarity l r) = withCompo l $ \ lc -> withCompo r $ \ rc -> do
  anExperiment "Bisimulation between" [l, r] $
    report (l, r) (bisimReport lc rc)
experiment (Print x) = withImplem x $ \ i -> do
  g <- use hasLens
  tell $ Seq.singleton
    $ ARawCode "Printing" x
    $ prettyUnelabed g i
experiment (Typing x) = withCompo x $ \ c -> do
  g <- use hasLens
  anExperiment "Typing for" [x] $
    prettyBlock
      $ runUnelab g
      $ TypeDecl x
          (getInputType <$> inpTys c)
          (getOutputType <$> oupTys c)
experiment (Display xs x) = withImplem x $ \ i -> do
  st <- use hasLens
  let (fdk, circuit) = whiteBoxDef st (map getName xs) i
  unless (null fdk) $ tell $ Seq.singleton (WhenDisplaying x $ toList fdk)
  case circuit of
    Nothing -> pure ()
    Just dot -> asks graphFormat >>= \ opts ->
      tell $ Seq.singleton $ case opts of
        SourceDot -> ADotGraph xs x dot
        RenderedSVG -> unsafePerformIO $
          findExecutable "dot" >>= \case
            Nothing -> pure (ANoExecutable "dot")
            Just{} -> AnSVGGraph xs x . lines <$> readProcess "dot" ["-q", "-Tsvg"] (unlines dot)
experiment (Dnf x) = withImplem x $ \ i -> do
  env <- use hasLens
  tell $ Seq.singleton
    $ ARawCode "Disjunctive Normal Form of" x
    $ prettyUnelabed env $ dnf env i
experiment (Anf x) = withImplem x $ \ i -> do
  env <- use hasLens
  tell $ Seq.singleton
    $ ARawCode "A Normal Form of" x
    $ prettyUnelabed env $ toANF i
experiment (Costing nms x) = do
  g <- use hasLens
  let support = foldMap singleton nms
  let cost = costing g support x
  anExperiment "Cost for" [x] $
    flip foldMapArr cost (\ (x, Sum k) ->
      let copies = "cop" ++ if k > 1 then "ies" else "y" in
      aLine $$ [pretty k, " ", pretty copies, " of ", identifier x])
experiment (Simplify x) = withImplem x $ \ i -> do
  g <- use hasLens
  tell $ Seq.singleton
    $ ARawCode "Simplification of" x
    $ prettyUnelabed g (deMorgan g i)
experiment (FromOutputs f xs bs) = do
  g <- use hasLens
  case ttToDef g f (map getInputName xs) bs of
    Nothing -> tell $ Seq.singleton (AnInvalidTruthTableOutput f)
    Just def -> do
      tell $ Seq.singleton
        $ ARawCode "DNF circuit for" f
        $ prettyUnelabed g def
