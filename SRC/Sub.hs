------------------------------------------------------------------------------
-----                                                                    -----
-----     Sub: Type alias Substitution for Syrup                         -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Syrup.SRC.Sub where

import Syrup.SRC.BigArray
import Syrup.SRC.Ty
import Syrup.SRC.Syn

import Data.Void
import Data.Maybe

class TySubst t where
  tySubst :: TyEnv -> t String -> Either String (t Void)

instance TySubst TY' where
  tySubst rho t = case t of
    BIT      -> pure BIT
    OLD t    -> OLD <$> tySubst rho t
    CABLE ts -> CABLE <$> mapM (tySubst rho) ts
    TYVAR x  -> case findArr x rho of
      Nothing -> Left x
      Just v  -> pure v

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

inlineAliases :: TyEnv -> [Either [String] (SourceC, String)] -> (TyEnv, [Either [String] (Source, String)])
inlineAliases rho []                     = (rho, [])
inlineAliases rho (Left s : tl)          = (Left s :) <$> inlineAliases rho tl
inlineAliases rho (Right (srcc, s) : tl) =
  case subAlias rho srcc of
    Right (rho, Left ty)   -> (Left log :)       <$> inlineAliases rho tl
      where log = ["Type alias " ++ ty ++ " defined." ]
    Right (rho, Right src) -> (Right (src, s) :) <$> inlineAliases rho tl
    Left x                 -> (Left err :)       <$> inlineAliases rho tl
      where err = ["You haven't defined the type alias " ++ x ++ " just now." , "" ]
