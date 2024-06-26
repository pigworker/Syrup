------------------------------------------------------------------------------
-----                                                                    -----
-----     Syn: Syntax for Syrup                                          -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Syn where

import Data.List (intercalate)
import Data.Void

import Language.Syrup.BigArray

data Source' a
  = Declaration (DEC' a)
  | TypeAlias (a, TY' a)
  | Definition Def
  | Experiment EXPT
  deriving Show

-- Concrete and internal sources
type SourceC = Source' String
type Source  = Source' Void

type Exp = Exp' ()
data Exp' ty
  = Var ty String
  | App [ty] String [Exp' ty]
  | Cab ty [Exp' ty]
  deriving (Eq, Functor, Foldable, Traversable)

type Pat = Pat' () String
data Pat' ty a
  = PVar ty a
  | PCab ty [Pat' ty a]
  deriving (Functor, Traversable, Foldable)

exPat :: Exp' ty -> Maybe (Pat' ty String)
exPat (Var ty x)  = return (PVar ty x)
exPat (Cab ty es) = PCab ty <$> traverse exPat es
exPat _           = Nothing

patToExp :: Pat' ty String -> Exp' ty
patToExp = \case
  PVar ty x  -> Var ty x
  PCab ty ps -> Cab ty $ map patToExp ps

type Eqn = Eqn' ()
data Eqn' ty = [Pat' ty String] :=: [Exp' ty]
type Def = Def' ()
data Def' ty
  = Stub String [String]
  -- stubbed out definition together with error msg
  | Def (String,[Pat' ty String]) [Exp' ty] (Maybe [Eqn' ty])

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
  | Display String
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

support :: Pat' ty String -> Set String
support (PVar _ x)  = singleton x
support (PCab _ ps) = foldMap support ps


------------------------------------------------------------------------------
-- ugly-printing
------------------------------------------------------------------------------

instance Show (Exp' ty) where
  show (Var _ x)    = x
  show (App _ f es) = concat [f, "(", csepShow es, ")"]
  show (Cab _ es)   = concat ["[", csepShow es, "]"]

instance a ~ String => Show (Pat' ty a) where
  show (PVar _ x)  = x
  show (PCab _ ps) = concat ["[", csepShow ps, "]"]

instance Show (Eqn' ty) where
  show (ps :=: es) = concat [csepShow ps, " = ", csepShow es]

instance Show (Def' ty) where
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
