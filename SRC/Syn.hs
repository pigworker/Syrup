------------------------------------------------------------------------------
-----                                                                    -----
-----     Syn: Syntax for Syrup                                          -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module Syrup.SRC.Syn where

import Data.List
import Data.Void

import Syrup.SRC.BigArray

data Source' a
  = Declaration (DEC' a)
  | TypeAlias (a, TY' a)
  | Definition Def
  | Experiment EXPT
  deriving Show

-- Concrete and internal sources
type SourceC = Source' String
type Source  = Source' Void

data Exp
  = Var String
  | App String [Exp]
  | Cab [Exp]
  deriving (Eq)

data Pat
  = PVar String
  | PCab [Pat]

exPat :: Exp -> Maybe Pat
exPat (Var x)   = return (PVar x)
exPat (Cab es)  = PCab <$> traverse exPat es
exPat _         = Nothing

patToExp :: Pat -> Exp
patToExp = \case
  PVar x  -> Var x
  PCab ps -> Cab $ map patToExp ps

data Eqn = [Pat] :=: [Exp]
data Def
  = Stub String [String]
  -- stubbed out definition together with error msg
  | Def (String,[Pat]) [Exp] (Maybe [Eqn])

data TY' a
  = BIT
  | OLD (TY' a)
  | CABLE [TY' a]
  | TYVAR a
  deriving Show

-- Concrete and internal types
type TYC = TY' String
type TY  = TY' Void

data DEC' a = DEC (String,[TY' a]) [TY' a]
  deriving Show

-- Concrete and internal declarations
type DECC = DEC' String
type DEC  = DEC' Void

data EXPT
  = Tabulate String
  | Simulate String [Va] [[Va]]
  | Bisimilarity String String
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
  show (Var x)    = x
  show (App f es) = concat [f, "(", csepShow es, ")"]
  show (Cab es)   = concat ["[", csepShow es, "]"]

instance Show Pat where
  show (PVar x) = x
  show (PCab ps) = concat ["[", csepShow ps, "]"]

instance Show Eqn where
  show (ps :=: es) = concat [csepShow ps, " = ", csepShow es]

instance Show Def where
  show = \case
    Stub{} -> "Stubbed out definition"
    Def (nm, ps) rhs eqns ->
      concat [ nm
             , "(", csepShow ps, ")"
             , " = ", csepShow rhs
             , flip (maybe "") eqns $ \ eqns ->
                 unlines (" where" : map (("  " ++) . show) eqns)
             ]

csepShow :: Show x => [x] -> String
csepShow = intercalate ", " . map show
