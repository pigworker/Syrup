------------------------------------------------------------------------------
-----                                                                    -----
-----     Syn: Syntax for Syrup                                          -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE
    DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Syrup.SRC.Syn where

import Data.List

import Syrup.SRC.BigArray

data Source
  = Declaration DEC
  | Definition Def
  | Experiment EXPT
  deriving Show

data Exp
  = Var String
  | App String [Exp]
  | Cab [Exp]

data Pat
  = PVar String
  | PCab [Pat]

exPat :: Exp -> Maybe Pat
exPat (Var x)   = return (PVar x)
exPat (Cab es)  = PCab <$> traverse exPat es
exPat _         = Nothing

data Eqn = [Pat] :=: [Exp]
data Def = Def (String,[Pat]) [Exp] [Eqn] deriving Show

data TY
  = BIT
  | OLD TY
  | CABLE [TY]
  deriving Show

data DEC = DEC (String,[TY]) [TY]
  deriving Show

data EXPT
  = Tabulate String
  | Simulate String [Va] [[Va]]
  deriving Show

data Va
  = V0 | V1 | VQ | VC [Va]
  deriving (Eq, Ord)

instance Show Va where
  show V0 = "0"
  show V1 = "1"
  show VQ = "?"
  show (VC vs) = "[" ++ foldMap show vs ++ "]"



------------------------------------------------------------------------------
-- operations on syntax
------------------------------------------------------------------------------

support :: Pat -> Set String
support (PVar x)  = singleton x
support (PCab ps) = foldMap support ps


------------------------------------------------------------------------------
-- ugly-printing
------------------------------------------------------------------------------

instance Show Exp where
  show (Var x) = x
  show (App f es) = concat [f, "(", csepShow es, ")"]
  show (Cab es) = concat ["[", csepShow es, "]"]

instance Show Pat where
  show (PVar x) = x
  show (PCab ps) = concat ["[", csepShow ps, "]"]

instance Show Eqn where
  show (ps :=: es) = concat [csepShow ps, " = ", csepShow es]

csepShow :: Show x => [x] -> String
csepShow = intercalate ", " . map show