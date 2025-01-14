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

import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Monoid (Sum(..), First(..))
import Data.Void (Void)

import Language.Syrup.BigArray
import Language.Syrup.Syn.Base
import Language.Syrup.Fdk

data Source' a
  = Declaration (DEC' a)
  | TypeAlias (a, TY' a)
  | Definition Def
  | Experiment EXPT

-- Concrete and internal sources
type SourceC = Source' String
type Source  = Source' Void

type Exp = Exp' () ()
data Exp' tys ty
  = Var ty String
  | Hol ty String
  | App tys String [Exp' tys ty]
  | Cab ty [Exp' tys ty]
  deriving (Eq, Functor, Foldable, Traversable)

expTys :: Exp' (NonEmpty ty) ty -> NonEmpty ty
expTys = \case
  Var ty _ -> ty :| []
  Hol ty _ -> ty :| []
  App tys _ _ -> tys
  Cab ty _ -> ty :| []

type Pat = Pat' () String
data Pat' ty a
  = PVar ty a
  | PCab ty [Pat' ty a]
  deriving (Functor, Traversable, Foldable)

patTy :: Pat' ty a -> ty
patTy = \case
  PVar ty a -> ty
  PCab ty _ -> ty

exPat :: Exp' tys ty -> Maybe (Pat' ty String)
exPat (Var ty x)  = return (PVar ty x)
exPat (Hol ty x)  = Nothing -- for now?
exPat (Cab ty es) = PCab ty <$> traverse exPat es
exPat _           = Nothing

patToExp :: Pat' ty String -> Exp' tys ty
patToExp = \case
  PVar ty x  -> Var ty x
  PCab ty ps -> Cab ty (patToExp <$> ps)

type Eqn = Eqn' () ()
data Eqn' tys ty = NonEmpty (Pat' ty String) :=: NonEmpty (Exp' tys ty)
type Def = Def' () ()
data Def' tys ty
  = Stub String [Feedback]
  -- stubbed out definition together with error msg
  | Def
      (String, [Pat' ty String])
      (NonEmpty (Exp' tys ty))
      (Either Bool (NonEmpty (Eqn' tys ty)))
      ---     ^^^^---- is this parsed as an empty where block?

toEqns :: [Eqn' tys ty] -> Either Bool (NonEmpty (Eqn' tys ty))
toEqns = maybe (Left False) Right . nonEmpty

fromEqns :: Either Bool (NonEmpty (Eqn' tys ty)) -> [Eqn' tys ty]
fromEqns = either (const []) toList

defName :: Def' tys ty -> String
defName (Stub f _) = f
defName (Def (f, _) _ _) = f

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

data InputName = InputName { getInputName :: String }
  deriving Show

data EXPT
  = Anf String
  | Bisimilarity String String
  | UnitTest String CircuitConfig CircuitConfig
  | Costing [String] String
  | Display [String] String
  | Dnf String
  | Print String
  | Simplify String
  | Simulate String [Va] [[Va]]
  | Typing String
  | Tabulate String
  | FromOutputs String [InputName] [Bool]
  deriving Show



------------------------------------------------------------------------------
-- operations on syntax
------------------------------------------------------------------------------

class IsCircuit t where
  type VarTy t :: Type
  allVars  :: t -> Arr String (First (VarTy t), Sum Int)
  allGates :: t -> Arr String (Sum Int)
  allHoles :: t -> Arr String (First (VarTy t))

  type TyFree t :: Type
  forgetTys :: t -> TyFree t

  default allVars
    :: (t ~ f a, VarTy t ~ VarTy a, Foldable f, IsCircuit a)
    => t -> Arr String (First (VarTy t), Sum Int)
  allVars = foldMap allVars

  default allGates
    :: (t ~ f a, Foldable f, IsCircuit a)
    => t -> Arr String (Sum Int)
  allGates = foldMap allGates

  default allHoles
    :: (t ~ f a, VarTy t ~ VarTy a, Foldable f, IsCircuit a)
    => t -> Arr String (First (VarTy t))
  allHoles = foldMap allHoles

  default forgetTys
    :: (t ~ f a, TyFree t ~ f (TyFree a), Traversable f, IsCircuit a)
    => t -> TyFree t
  forgetTys = fmap forgetTys

instance IsCircuit a => IsCircuit [a] where
  type VarTy [a] = VarTy a
  type TyFree [a] = [TyFree a]
instance IsCircuit a => IsCircuit (NonEmpty a) where
  type VarTy (NonEmpty a) = VarTy a
  type TyFree (NonEmpty a) = NonEmpty (TyFree a)
instance IsCircuit a => IsCircuit (Maybe a) where
  type VarTy (Maybe a) = VarTy a
  type TyFree (Maybe a) = Maybe (TyFree a)
instance IsCircuit a => IsCircuit (Either e a) where
  type VarTy (Either e a) = VarTy a
  type TyFree (Either e a) = Either e (TyFree a)

instance a ~ String => IsCircuit (Pat' ty a) where
  type VarTy (Pat' ty a) = ty
  allVars = \case
    PVar ty s -> single (s, (First (Just ty), Sum 1))
    PCab _ c -> allVars c
  allGates _ = emptyArr
  allHoles _ = emptyArr

  type TyFree (Pat' ty a) = Pat' () a
  forgetTys = \case
    PVar _ s -> PVar () s
    PCab _ c -> PCab () (forgetTys c)

instance IsCircuit (Def' tys ty) where
  type VarTy (Def' tys ty) = ty
  allVars = \case
    Stub{} -> emptyArr
    Def (fn,ps) es meqns -> allVars ps <> allVars es <> allVars meqns
  allGates = \case
    Stub{} -> emptyArr
    Def (fn,ps) es meqns -> allGates es <> allGates meqns
  allHoles = \case
    Stub{} -> emptyArr
    Def (fn,ps) es meqns -> allHoles es <> allHoles meqns

  type TyFree (Def' tys ty) = Def' () ()
  forgetTys = \case
    Stub fn fdks -> Stub fn fdks
    Def lhs rhs meqns -> Def (forgetTys <$> lhs) (forgetTys rhs) (forgetTys meqns)

instance IsCircuit (Exp' tys ty) where
  type VarTy (Exp' tys ty) = ty
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

  type TyFree (Exp' tys ty) = Exp' () ()
  forgetTys = \case
    Var _ x -> Var () x
    Hol _ x -> Hol () x
    App _ fn es -> App () fn (forgetTys es)
    Cab _ es -> Cab () (forgetTys es)

instance IsCircuit (Eqn' tys ty) where
  type VarTy (Eqn' tys ty) = ty
  allVars (ps :=: es) = allVars ps <> allVars es
  allGates (ps :=: es) = allGates es
  allHoles (ps :=: es) = allHoles es

  type TyFree (Eqn' tys ty) = Eqn' () ()
  forgetTys (ps :=: es) = forgetTys ps :=: forgetTys es

support :: IsCircuit t => t -> Set String
support p = () <$ allVars p
