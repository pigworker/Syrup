------------------------------------------------------------------------------
-----                                                                    -----
-----     DeMorgan: Simplifying circuits using laws                      -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Language.Syrup.DeMorgan where

import Control.Monad (guard)
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe (fromMaybe)

import Language.Syrup.BigArray
import Language.Syrup.Ded
import Language.Syrup.Syn
import Language.Syrup.Ty
import Language.Syrup.Utils (partitionWith)

import Debug.Trace

------------------------------------------------------------------------
-- Deploying De Morgan's laws to simplify circuits
------------------------------------------------------------------------

deMorgan :: CoEnv -> Def' ty -> Def' ty
deMorgan env (Def lhs rhs meqns) =
  let simpl = simplify Positive rhs in
  let (rhs', eqns') = runReader (runStateT simpl (fromMaybe [] meqns)) env in
  cleanup $ Def lhs rhs' (eqns' <$ guard (not (null eqns')))
deMorgan env d = d

data Polarity ty
  = Positive
  | Negative String ty
  -- ^ this is storing the names of the:
  -- 1. the not gate
  -- 2. the Bit type

isPositive :: Polarity ty -> Bool
isPositive Positive = True
isPositive _ = False

instance Show (Polarity ty) where
  show Positive = "+"
  show Negative{} = "-"

inverse :: String -> ty -> Polarity ty -> Polarity ty
inverse nm ty Positive = Negative nm ty
inverse _ _ (Negative _ _) = Positive

type DeMorganM ty = StateT [Eqn' ty] (Reader CoEnv)

class DeMorgan ty t where
  simplify :: Polarity ty -> t -> DeMorganM ty t

isRemarkable :: String -> DeMorganM ty (Maybe Remarkable)
isRemarkable f = asks (findArr f >=> rmk)

isAssignment :: String -> Eqn' ty -> Either (Exp' ty) (Eqn' ty)
isAssignment x eqn@([PVar _ y] :=: [e])
  | x == y = Left e
  | otherwise = Right eqn
isAssignement x eqn = Right eqn

isDefined :: String -> DeMorganM ty (Maybe (Exp' ty))
isDefined x = do
  eqns <- get
  case partitionWith (isAssignment x) eqns of
    ([e], es) -> Just e <$ put es
    _ -> pure Nothing

applyPolarity :: Polarity ty -> Exp' ty -> Exp' ty
applyPolarity Positive e = e
applyPolarity (Negative fn ty) e = App [ty] fn [e]

instance DeMorgan ty (Exp' ty) where
  simplify pol (App [ty] fn [e]) = isRemarkable fn >>= \case
    Just IsNotGate -> simplify (inverse fn ty pol) e
    _ -> do e <- simplify Positive e
            pure $ applyPolarity pol (App [ty] fn [e])
  simplify pol og@(Var ty x) = isDefined x >>= \case
    Just e -> do e <- simplify Positive e
                 modify (([PVar ty x] :=: [e]) :)
                 if isPositive pol then pure og else simplify pol e
    Nothing -> pure $ applyPolarity pol og
  simplify pol og = pure $ applyPolarity pol og

instance DeMorgan ty a => DeMorgan ty [a] where
  simplify pol = traverse (simplify pol)
