------------------------------------------------------------------------------
-----                                                                    -----
-----     DeMorgan: Simplifying circuits using laws                      -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Language.Syrup.DeMorgan where

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State

import Data.List.NonEmpty (NonEmpty(..))

import Language.Syrup.Ded
import Language.Syrup.Syn
import Language.Syrup.Ty
import Language.Syrup.Utils (partitionWith)

------------------------------------------------------------------------
-- Deploying De Morgan's laws to simplify circuits
-- Trying to be clever about circuit depth
------------------------------------------------------------------------

deMorgan :: CoEnv -> Def' (NonEmpty ty) ty -> Def' (NonEmpty ty) ty
deMorgan env (Def lhs rhs meqns) =
  let simpl = simplify Positive rhs in
  let (rhs', eqns') = runReader (runStateT simpl (fromEqns meqns)) env in
  cleanup $ Def lhs rhs' (toEqns eqns')
deMorgan env d = d

data Polarity ty
  = Positive
  | Negative String ty
  -- ^ this is storing the names of:
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

type DeMorganM ty = StateT [Eqn' (NonEmpty ty) ty] (Reader CoEnv)

class DeMorgan ty t where
  simplify :: Polarity ty -> t -> DeMorganM ty t

isAssignment :: String -> Eqn' tys ty -> Either (Exp' tys ty) (Eqn' tys ty)
isAssignment x eqn@((PVar _ y :| []) :=: (e :| []))
  | x == y = Left e
  | otherwise = Right eqn
isAssignment x eqn = Right eqn

isDefined :: String -> DeMorganM ty (Maybe (Exp' (NonEmpty ty) ty))
isDefined x = do
  eqns <- get
  case partitionWith (isAssignment x) eqns of
    ([e], es) -> Just e <$ put es
    _ -> pure Nothing

applyPolarity :: Polarity ty -> Exp' (NonEmpty ty) ty -> Exp' (NonEmpty ty) ty
applyPolarity Positive e = e
applyPolarity (Negative fn ty) e = App (ty :| []) fn [e]

mkIdempotent :: tys -> String -> Exp' tys ty -> Exp' tys ty -> Exp' tys ty
mkIdempotent tys fn e1 e2
  | forgetTys e1 == forgetTys e2 = e1
  | otherwise = App tys fn [e1, e2]

instance DeMorgan ty (Exp' (NonEmpty ty) ty) where
  simplify pol (App (ty :| []) fn [e]) = isRemarkable fn >>= \case
    Just IsNotGate -> simplify (inverse fn ty pol) e
    _ -> do
      e <- simplify Positive e
      pure $ applyPolarity pol (App (ty :| []) fn [e])
  simplify pol (App (ty :| []) fn [e1,e2]) =
    let structural = do
          e1 <- simplify Positive e1
          e2 <- simplify Positive e2
          pure $ applyPolarity pol (App (ty :| []) fn [e1, e2])
    in isRemarkable fn >>= \case
    Just IsAndGate | not (isPositive pol) -> do
      e1 <- simplify Positive e1
      e2 <- simplify Positive e2
      pure $ App (ty :| []) "nand" [e1, e2]
    Just IsOrGate | not (isPositive pol) ->
      getRemarkable IsAndGate >>= \case
        Just and -> do
          e1 <- simplify pol e1
          e2 <- simplify pol e2
          pure $ mkIdempotent (ty :| []) and e1 e2
        _ -> structural
    Just IsOrGate | otherwise -> do
      e1 <- simplify Positive e1
      e2 <- simplify Positive e2
      let dflt = mkIdempotent (ty :| []) fn e1 e2
      case (e1, e2) of
        (App (_ :| []) ln [e1'], App (_ :| []) rn [e2']) -> do
          rmkl <- isRemarkable ln
          rmkr <- isRemarkable rn
          pure $ case (,) <$> rmkl <*> rmkr of
            Just (IsNotGate, IsNotGate) -> App (ty :| []) "nand" [e1', e2']
            _ -> dflt
        (App (_ :| []) ln [e11, e12], App (_ :| []) rn [e2']) -> do
          rmkl <- isRemarkable ln
          rmkr <- isRemarkable rn
          mand <- getRemarkable IsAndGate
          pure $ case (,,) <$> mand <*> rmkl <*> rmkr of
            Just (and, IsNandGate, IsNotGate) ->
              App (ty :| []) "nand" [mkIdempotent (ty :| []) and e11 e12, e2']
            _ -> dflt
        (App (_ :| []) ln [e1'], App (_ :| []) rn [e21, e22]) -> do
          rmkl <- isRemarkable ln
          rmkr <- isRemarkable rn
          mand <- getRemarkable IsAndGate
          pure $ case (,,) <$> mand <*> rmkl <*> rmkr of
            Just (and, IsNotGate, IsNandGate) ->
              App (ty :| []) "nand" [e1', mkIdempotent (ty :| []) and e21 e22]
            _ -> dflt
        _ -> pure dflt
    _ -> structural
  simplify pol og@(Var ty x) = isDefined x >>= \case
    Just e -> do
      e <- simplify Positive e
      modify (((PVar ty x :| []) :=: (e :| [])) :)
      if isPositive pol then pure og else simplify pol e
    Nothing -> pure $ applyPolarity pol og
  simplify pol og = pure $ applyPolarity pol og

instance DeMorgan ty a => DeMorgan ty [a] where
  simplify pol = traverse (simplify pol)

instance DeMorgan ty a => DeMorgan ty (NonEmpty a) where
  simplify pol = traverse (simplify pol)
