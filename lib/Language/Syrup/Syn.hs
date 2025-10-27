------------------------------------------------------------------------------
-----                                                                    -----
-----     Syn: Syntax for Syrup                                          -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Syrup.Syn
  ( module Language.Syrup.Syn.Base
  , module Language.Syrup.Syn
  ) where

import Data.IMaybe (IMaybe(..))
import Data.Kind (Type)
import Data.Monoid (Sum(..), First(..))
import Data.Void (Void)

import Language.Syrup.BigArray
import Language.Syrup.Syn.Base
import Language.Syrup.Fdk

data Source' a b
  = Declaration (DEC' b)
  | TypeAlias (a, TY' b)
  | Definition Def
  | Experiment EXPT

-- Concrete and internal sources
type SourceC = Source' TyName False
type Source  = Source' Void   True

type Exp = Exp' ()
data Exp' ty
  = Var ty String
  | Hol ty String
  | App [ty] Name [Exp' ty]
  | Cab ty [Exp' ty]
  deriving (Eq, Functor, Foldable, Traversable)

expTys :: Exp' ty -> [ty]
expTys = \case
  Var ty _ -> [ty]
  Hol ty _ -> [ty]
  App tys _ _ -> tys
  Cab ty _ -> [ty]

type Pat = Pat' () String
data Pat' ty a
  = PVar ty a
  | PCab ty [Pat' ty a]
  deriving (Functor, Traversable, Foldable)

patTy :: Pat' ty a -> ty
patTy = \case
  PVar ty a -> ty
  PCab ty _ -> ty

exPat :: Exp' ty -> Maybe (Pat' ty String)
exPat (Var ty x)  = return (PVar ty x)
exPat (Hol ty x)  = Nothing -- for now?
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
  = Stub Name [Feedback]
  -- stubbed out definition together with error msg
  | Def (Name, [Pat' ty String]) [Exp' ty] (Maybe [Eqn' ty])

defName :: Def' ty -> Name
defName (Stub f _) = f
defName (Def (f, _) _ _) = f

data TY' b
  = BIT
  | OLD (TY' b)
  | CABLE [TY' b]
  | TYVAR TyName (IMaybe b (TY' b))
  deriving (Show)

-- Concrete and internal types
type TYC = TY' False
type TY  = TY' True

data DEC' b = DEC (Name,[TY' b]) [TY' b]
  deriving Show

-- Concrete and internal declarations
type DECC = DEC' False
type DEC  = DEC' True

data InputName = InputName { getInputName :: String }
  deriving Show

data EXPT
  = Anf Name
  | Bisimilarity Name Name
  | UnitTest Name CircuitConfig CircuitConfig
  | Costing [Name] Name
  | Display [Name] Name
  | Dnf Name
  | Print Name
  | Simplify Name
  | Simulate Name [Va] [[Va]]
  | Typing Name
  | Tabulate Name
  | FromOutputs Name [InputName] [Bool]
  deriving Show



------------------------------------------------------------------------------
-- operations on syntax
------------------------------------------------------------------------------

class IsCircuit t where
  type VarTy t :: Type
  allVars  :: t -> Arr String (First (VarTy t), Sum Int)
  allGates :: t -> Arr Name (Sum Int)
  allHoles :: t -> Arr String (First (VarTy t))

  default allVars
    :: (t ~ f a, VarTy t ~ VarTy a, Foldable f, IsCircuit a)
    => t -> Arr String (First (VarTy t), Sum Int)
  allVars = foldMap allVars

  default allGates
    :: (t ~ f a, Foldable f, IsCircuit a)
    => t -> Arr Name (Sum Int)
  allGates = foldMap allGates

  default allHoles
    :: (t ~ f a, VarTy t ~ VarTy a, Foldable f, IsCircuit a)
    => t -> Arr String (First (VarTy t))
  allHoles = foldMap allHoles

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
  allHoles _ = emptyArr

instance IsCircuit (Def' ty) where
  type VarTy (Def' ty) = ty
  allVars = \case
    Stub{} -> emptyArr
    Def (fn,ps) es meqns -> allVars ps <> allVars es <> allVars meqns
  allGates = \case
    Stub{} -> emptyArr
    Def (fn,ps) es meqns -> allGates es <> allGates meqns
  allHoles = \case
    Stub{} -> emptyArr
    Def (fn,ps) es meqns -> allHoles es <> allHoles meqns

instance IsCircuit (Exp' ty) where
  type VarTy (Exp' ty) = ty
  allVars = \case
    Var ty x -> single (x, (First (Just ty), Sum 1))
    Hol ty x -> emptyArr
    App _ fn es -> allVars es
    Cab _ es -> allVars es
  allGates = \case
    Var{} -> emptyArr
    Hol{} -> emptyArr
    Cab{} -> emptyArr
    App _ fn es -> single (fn, Sum 1) <> allGates es
  allHoles = \case
    Var ty x -> emptyArr
    Hol ty x -> single (x, First (Just ty))
    App _ fn es -> allHoles es
    Cab _ es -> allHoles es

instance IsCircuit (Eqn' ty) where
  type VarTy (Eqn' ty) = ty
  allVars (ps :=: es) = allVars ps <> allVars es
  allGates (ps :=: es) = allGates es
  allHoles (ps :=: es) = allHoles es

support :: IsCircuit t => t -> Set String
support p = () <$ allVars p
