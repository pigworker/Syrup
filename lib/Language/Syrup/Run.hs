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
import Control.Monad.Writer (tell, listens, runWriter)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.List (intercalate)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Language.Syrup.Chk
import Language.Syrup.Dot
import Language.Syrup.Expt
import Language.Syrup.Fdk
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

getDefsOf :: String -> [Either a (Source, String)] -> [(Def, String)]
getDefsOf f src = src >>= \case
  Right (Definition def@(Def (g, _) _ _), s) | g == f -> [(def,s)]
  Right (Definition def@(Stub g _)      , s) | g == f -> [(def,s)]
  _ -> []

grokSy :: MonadExperiment s m
       => [Either Feedback (Source, String)]
       -> m ()
grokSy [] = pure ()
grokSy (Left ss : src) = do
  tell $ Seq.singleton ss
  grokSy src
grokSy (Right (Declaration dec@(DEC (f, _) _), s) : src) = do
  let (warn, rest)  = spanMaybe isLeft src
  mapM_ (tell . Seq.singleton) warn
  mdef <- case getDefsOf f rest of
    [defs] -> pure (Just defs)
    zs@(_ : _ : _) -> do
      tell $ Seq.singleton $ Ambiguous f (map (lines . snd) zs)
      pure Nothing
    [] -> do
      tell $ Seq.singleton $ Undefined f
      pure Nothing
  (_, mtydef) <- mkComponent' True (dec, s) mdef
  whenJust mtydef $ (hasLens %=) . flip addDef
  grokSy rest
grokSy (Right (Experiment expt, _) : src) = do
  experiment expt
  grokSy src
grokSy (Right (Definition d, _) : src) =
  grokSy src

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
oldRunSyrup :: Options -> SyrupEnv -> String -> (SyrupEnv, String)
oldRunSyrup opts env src
  = fmap (unlines . feedback opts . toList)
  $ runWriter
  $ flip runReaderT opts
  $ flip execStateT env
  $ runSyrup src


syrup :: Options -> String -> String
syrup opts src = snd $ oldRunSyrup opts (SyrupEnv myTyEnv myCoEnv myDotSt) src
