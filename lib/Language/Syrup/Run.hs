------------------------------------------------------------------------------
-----                                                                    -----
-----     Run: Running Syrup on a default environment                    -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Syrup.Run where

import Control.Monad.State (execStateT)
import Control.Monad.Writer (tell, runWriter)
import Control.Monad.Reader (runReaderT)

import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Functor (void)
import qualified Data.Sequence as Seq
import Data.Void (absurd)

import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Language.Syrup.Chk
import Language.Syrup.Dot
import Language.Syrup.Expt
import Language.Syrup.Fdk (Feedback(..), feedbackText, feedbackHtml, keep)
import Language.Syrup.Lnt
import Language.Syrup.Opt
import Language.Syrup.Par
import Language.Syrup.Scp
import Language.Syrup.Sub
import Language.Syrup.Syn
import Language.Syrup.Ty
import Language.Syrup.Utils

import Utilities.Lens (Has, hasLens, (.=), use, (%=))
import Utilities.Monad (whenJust)

getDefsOf :: String
          -> [Either a (Source, String)]
          -> ([Either a (Source, String)], [(Def, String)])
getDefsOf f xs = partitionEithers $ flip map xs $ \case
  Right (Definition def, s) | defName def == f -> Right (def,s)
  src -> Left src

grokSy :: MonadExperiment s m
       => [Either Feedback (Source, String)]
       -> m ()
grokSy [] = pure ()
grokSy (Left ss : src) = do
  tell $ Seq.singleton ss
  grokSy src
grokSy (Right (Declaration dec@(DEC (f, _) _), s) : src) = do
  let (warn, rest0)  = spanMaybe isLeft src
  mapM_ (tell . Seq.singleton) warn
  let (rest, defs) = getDefsOf f rest0
  mdef <- case defs of
    [def] -> pure (Just def)
    zs@(_ : _ : _) -> do
      tell $ Seq.singleton $ AnAmbiguousDefinition f (map (lines . snd) zs)
      pure Nothing
    [] -> do
      tell $ Seq.singleton $ AnUndefinedCircuit f
      pure Nothing
  (_, mtydef) <- mkComponent' True (dec, s) mdef
  whenJust mtydef $ (hasLens %=) . flip addDef
  grokSy rest
grokSy (Right (Experiment expt, _) : src) = do
  expt' <- checkExperiment expt
  experiment expt'
  grokSy src
grokSy (Right (Definition d, _) : src) = do
  tell $ Seq.singleton $ AnUndeclaredCircuit (defName d)
  grokSy src
grokSy (Right (TypeAlias (x, _), _) : src) = absurd x

data SyrupEnv = SyrupEnv
  { syrupTyEnv :: TyEnv
  , syrupCoEnv :: CoEnv
  , syrupDotSt :: DotSt
  }

instance Has TyEnv SyrupEnv where
  hasLens f (SyrupEnv ty co dot) = (\ ty -> SyrupEnv ty co dot) <$> f ty

instance Has CoEnv SyrupEnv where
  hasLens f (SyrupEnv ty co dot) = (\ co -> SyrupEnv ty co dot) <$> f co

instance Has DotSt SyrupEnv where
  hasLens f (SyrupEnv ty co dot) = (\ dot -> SyrupEnv ty co dot) <$> f dot

type MonadSyrup s m =
  ( Has TyEnv s
  , MonadExperiment s m
  )

runSyrup :: MonadSyrup s m => String -> m ()
runSyrup txt = do
  let ls = syrupFile txt
  let linted = linter ls
  g <- use hasLens
  let scps = check (globalScope (void (g :: CoEnv))) linted
  t <- use hasLens
  -- TODO: mtl-ise inlineAliases?
  let (t', srcs) = inlineAliases t scps
  hasLens .= t'
  grokSy srcs

-- The sort of interface Marx prefers
oldRunSyrup
  :: ([Feedback] -> a)
  -> Options -> SyrupEnv -> String -> (SyrupEnv, a)
oldRunSyrup fdk opts env src
  = fmap (fdk . filter (keep opts) . toList)
  $ runWriter
  $ flip runReaderT opts
  $ flip execStateT env
  $ runSyrup src

marxRunSyrup :: Options -> SyrupEnv -> String -> (SyrupEnv, Html)
marxRunSyrup = oldRunSyrup feedbackHtml

syrup :: Options -> String -> String
syrup opts src = snd $ oldRunSyrup fdk opts env src where

  env :: SyrupEnv
  env = SyrupEnv myTyEnv myCoEnv myDotSt

  fdk :: [Feedback] -> String
  fdk = case outputFormat opts of
    TextOutput -> unlines . feedbackText
    HtmlOutput -> renderHtml . feedbackHtml
