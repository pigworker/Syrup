------------------------------------------------------------------------------
-----                                                                    -----
-----     Syn: Syntax for Syrup                                          -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE
    DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Syn where

data Exp
  = Var String
  | App String [Exp]
  | Cab [Exp]
  deriving Show

data Pat
  = PVar String
  | PCab [Pat]
  deriving Show

exPat :: Exp -> Maybe Pat
exPat (Var x)   = return (PVar x)
exPat (Cab es)  = PCab <$> traverse exPat es
exPat _         = Nothing

data Eqn = [Pat] :=: [Exp] deriving Show
data Def = Def (String,[Pat]) [Exp] [Eqn] deriving Show

data TY
  = BIT
  | OLD TY
  | CABLE [TY]
  deriving Show

data Dec t = Dec (String,[t]) [t]
  deriving (Show, Functor, Foldable, Traversable)

