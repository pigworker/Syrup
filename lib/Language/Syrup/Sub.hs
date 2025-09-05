------------------------------------------------------------------------------
-----                                                                    -----
-----     Sub: Type alias Substitution for Syrup                         -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.Syrup.Sub where

import Data.IMaybe (IMaybe(IJust))

import Language.Syrup.BigArray
import Language.Syrup.Fdk
import Language.Syrup.Syn
import Language.Syrup.Ty

class TySubst t where
  tySubst :: TyEnv -> t False -> Either String (t True)

instance TySubst TY' where
  tySubst rho t = case t of
    BIT      -> pure BIT
    OLD t    -> OLD <$> tySubst rho t
    CABLE ts -> CABLE <$> mapM (tySubst rho) ts
    TYVAR x _ -> case findArr x rho of
      Nothing -> Left x
      Just v  -> pure $ TYVAR x (IJust v)

instance TySubst DEC' where
  tySubst rho (DEC (str, ts) us) =
    DEC <$> ((str,) <$> mapM (tySubst rho) ts)
        <*> mapM (tySubst rho) us

subAlias :: TyEnv -> SourceC -> Either String (TyEnv, Either String Source)
subAlias rho c = case c of
  Declaration d    -> (rho,) . pure . Declaration <$> tySubst rho d
  TypeAlias (n, t) -> do
    t' <- tySubst rho t
    pure (insertArr (n , t') rho, Left n)
  Definition d -> pure (rho, pure (Definition d))
  Experiment e -> pure (rho, pure (Experiment e))

inlineAliases :: TyEnv
              -> [Either Feedback (SourceC, String)]
              -> (TyEnv, [Either Feedback (Source, String)])
inlineAliases rho []                     = (rho, [])
inlineAliases rho (Left s : tl)          = (Left s :) <$> inlineAliases rho tl
inlineAliases rho (Right (srcc, s) : tl) =
  case subAlias rho srcc of
    Right (rho, Left ty)   -> (Left (ATypeDefined ty) :)   <$> inlineAliases rho tl
    Right (rho, Right src) -> (Right (src, s) :)           <$> inlineAliases rho tl
    Left x                 -> (Left (AnUndefinedType x) :) <$> inlineAliases rho tl
