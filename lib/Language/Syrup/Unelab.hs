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

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Void (Void)

import Language.Syrup.Syn
import Language.Syrup.Ty


------------------------------------------------------------------------
-- Main utility


runUnelab :: Unelab s => CoEnv -> s -> Unelabed s
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

class Unelab s where
  type Unelabed s :: Type
  unelab :: MonadUnelab m => s -> m (Unelabed s)

  default unelab
    :: (s ~ f s', Unelabed (f s') ~ f (Unelabed s'), Traversable f, Unelab s', MonadUnelab m)
    => s -> m (Unelabed s)
  unelab = traverse unelab

------------------------------------------------------------------------
-- Unelab instances

instance Unelab Name where
  type Unelabed Name = PrettyName
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

instance Unelab Integer where
  type Unelabed Integer = Integer
  unelab = pure

instance Unelab Void where
  type Unelabed Void = Void
  unelab = pure

instance Unelab () where
  type Unelabed () = ()
  unelab = pure

instance Unelab s => Unelab [s] where
  type Unelabed [s] = [Unelabed s]
instance Unelab s => Unelab (Maybe s) where
  type Unelabed (Maybe s) = Maybe (Unelabed s)

instance Unelab (Exp' Name ty) where
  type Unelabed (Exp' Name ty) = Exp' PrettyName ty
  unelab = \case
    Var ty x -> pure $ Var ty x
    Hol ty x -> pure $ Hol ty x
    Cab tys es -> Cab tys <$> unelab es
    App tys f es -> App tys <$> unelab f <*> unelab es

instance Unelab (Pat' ty a) where
  type Unelabed (Pat' ty a) = Pat' ty a
  unelab = pure

instance Unelab (Ty ty x) where
  type Unelabed (Ty ty x) = Ty ty x
  unelab = pure

instance Unelab (Eqn' Name ty) where
  type Unelabed (Eqn' Name ty) = Eqn' PrettyName ty
  unelab (ps :=: es) = (ps :=:) <$> unelab es

instance Unelab (Def' Name ty) where
  type Unelabed (Def' Name ty) = Def' PrettyName ty
  unelab = \case
    Stub nm fdk -> Stub <$> unelab nm <*> pure fdk
    Def (nm, lhs) rhs meqn -> Def . (, lhs) <$> unelab nm <*> unelab rhs <*> unelab meqn

instance Unelab (TypeDecl' Name t x t' x') where
  type Unelabed (TypeDecl' Name t x t' x') = TypeDecl' PrettyName t x t' x'
  unelab (TypeDecl fn is os) = TypeDecl <$> unelab fn <*> pure is <*> pure os
