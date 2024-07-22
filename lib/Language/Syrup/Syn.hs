------------------------------------------------------------------------------
-----                                                                    -----
-----     Syn: Syntax for Syrup                                          -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Syrup.Syn where

import Data.Kind (Type)
import Data.List (intercalate)
import Data.Monoid (Sum(..), First(..))
import Data.Void (Void)

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
  | Anf String
  | Costing [String] String
  | Simplify String
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

class IsCircuit t where
  type VarTy t :: Type
  allVars  :: t -> Arr String (First (VarTy t), Sum Int)
  allGates :: t -> Arr String (Sum Int)

  default allVars
    :: (t ~ f a, VarTy t ~ VarTy a, Foldable f, IsCircuit a)
    => t -> Arr String (First (VarTy t), Sum Int)
  allVars = foldMap allVars

  default allGates
    :: (t ~ f a, Foldable f, IsCircuit a)
    => t -> Arr String (Sum Int)
  allGates = foldMap allGates

instance IsCircuit a => IsCircuit [a] where
  type VarTy [a] = VarTy a
instance IsCircuit a => IsCircuit (Maybe a) where
  type VarTy (Maybe a) = VarTy a

instance a ~ String => IsCircuit (Pat' ty a) where
  type VarTy (Pat' ty a) = ty
  allVars = \case
    PVar ty s -> single (s, (First (Just ty), Sum 1))
    PCab _ c -> allVars c
  allGates _ = emptyArr

instance IsCircuit (Def' ty) where
  type VarTy (Def' ty) = ty
  allVars = \case
    Stub{} -> emptyArr
    Def (fn,ps) es meqns -> allVars ps <> allVars es <> allVars meqns
  allGates = \case
    Stub{} -> emptyArr
    Def (fn,ps) es meqns -> allGates es <> allGates meqns

instance IsCircuit (Exp' ty) where
  type VarTy (Exp' ty) = ty
  allVars = \case
    Var ty x -> single (x, (First (Just ty), Sum 1))
    App _ fn es -> allVars es
    Cab _ es -> allVars es
  allGates = \case
    Var{} -> emptyArr
    Cab{} -> emptyArr
    App _ fn es -> single (fn, Sum 1) <> allGates es

instance IsCircuit (Eqn' ty) where
  type VarTy (Eqn' ty) = ty
  allVars (ps :=: es) = allVars ps <> allVars es
  allGates (ps :=: es) = allGates es

support :: IsCircuit t => t -> Set String
support p = () <$ allVars p

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
