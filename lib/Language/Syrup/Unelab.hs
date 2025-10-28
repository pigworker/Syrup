------------------------------------------------------------------------------
-----                                                                    -----
-----     Unelab: Unelaboration for Syrup                                -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Syrup.Unelab where

import Control.Monad.Reader (MonadReader, runReader)

import Data.Maybe (fromMaybe)
import Data.Void (Void)

import Language.Syrup.Syn
import Language.Syrup.Ty


------------------------------------------------------------------------
-- Main utility


runUnelab :: Unelab s t => CoEnv -> s -> t
runUnelab env = flip runReader env . unelab


------------------------------------------------------------------------
-- Names of unelabed definitions

data PrettyName
  = StandardName Name          -- printed e.g. f(X,Y,Z)
  | RemarkableName Remarkable  -- printed e.g. X & Y

toName :: PrettyName -> Name
toName (StandardName nm) = nm
toName (RemarkableName r) = Name $ case r of
  IsZeroGate -> "zero"
  IsOneGate -> "one"
  IsNotGate -> "not"
  IsAndGate -> "and"
  IsOrGate -> "or"
  IsNandGate -> "nand"

------------------------------------------------------------------------
-- Unelab monad and class

type MonadUnelab m =
  (MonadReader CoEnv m)

class Unelab s t where
  unelab :: MonadUnelab m => s -> m t

  default unelab
    :: (s ~ f s', t ~ f t', Traversable f, Unelab s' t', MonadUnelab m)
    => s -> m t
  unelab = traverse unelab

------------------------------------------------------------------------
-- Unelab instances

instance Unelab Name PrettyName where
  unelab nm = isRemarkable nm >>= \ mrem ->
    pure $ fromMaybe (StandardName nm) $ do
      rem <- mrem
      let success = Just (RemarkableName rem)
      case rem of
        IsZeroGate | nm == Name "zero" -> success
        IsOneGate  | nm == Name "one"  -> success
        IsNotGate  | nm == Name "not"  -> success
        IsAndGate  | nm == Name "and"  -> success
        IsOrGate   | nm == Name "or"   -> success
        _ -> Nothing

instance Unelab Integer Integer where
  unelab = pure

instance Unelab Void Void where
  unelab = pure

instance Unelab () () where
  unelab = pure

instance Unelab s t => Unelab [s] [t] where
instance Unelab s t => Unelab (Maybe s) (Maybe t) where

instance Unelab (Exp' Name ty) (Exp' PrettyName ty) where
  unelab = \case
    Var ty x -> pure $ Var ty x
    Hol ty x -> pure $ Hol ty x
    Cab tys es -> Cab tys <$> unelab es
    App tys f es -> App tys <$> unelab f <*> unelab es

instance Unelab (Eqn' Name ty) (Eqn' PrettyName ty) where
  unelab (ps :=: es) = (ps :=:) <$> unelab es

instance Unelab (Def' Name ty) (Def' PrettyName ty) where
  unelab = \case
    Stub nm fdk -> Stub <$> unelab nm <*> pure fdk
    Def (nm, lhs) rhs meqn -> Def . (, lhs) <$> unelab nm <*> unelab rhs <*> unelab meqn

instance Unelab (TypeDecl' Name t x t' x') (TypeDecl' PrettyName t x t' x') where
  unelab (TypeDecl fn is os) = TypeDecl <$> unelab fn <*> pure is <*> pure os
