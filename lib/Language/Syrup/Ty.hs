------------------------------------------------------------------------------
-----                                                                    -----
-----     Ty: types for Syrup                                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Syrup.Ty where

import Control.Applicative ((<|>))
import Control.Monad (guard, (>=>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks, runReader)
import Control.Monad.State (MonadState, get, gets, put)
import Control.Monad.Writer (MonadWriter)

import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.Forget (forget)
import Data.Monoid (First(..))
import Data.Sequence (Seq)
import Data.Void (Void, absurd)

import Language.Syrup.BigArray
import Language.Syrup.Bwd
import Language.Syrup.Fdk
import Language.Syrup.Syn
import Language.Syrup.Va

import Utilities.Lens

------------------------------------------------------------------------------
-- representing Syrup types
------------------------------------------------------------------------------

type Ty1 = Ty Unit Void
type Ty2 = Ty Ti Void
type Typ = Ty Unit TyNom

type TypedPat = Pat' Typ String
type TypedExp = Exp' Typ
type TypedEqn = Eqn' Typ
type TypedDef = Def' Typ

sizeBits :: Ty a Void -> Int
sizeBits = \case
  Meta v   -> absurd v
  TVar _ t -> sizeBits t
  Bit{}    -> 1
  Cable ts -> sum (sizeBits <$> ts)

-- TODO: check exact usage
sizeTy :: Ty a Void -> Int
sizeTy = \case
  Meta v   -> absurd v
  TVar _ t -> sizeTy t
  Bit{}    -> 1
  Cable ts -> 2 + sum (sizeTy <$> ts)

newtype CellName = CellName { cellName :: String }
  deriving (Eq, Show)

data MemoryCell = MemoryCell
  { getCellName :: Maybe CellName
  , getCellType :: Ty1
  }

data InputWire = InputWire
  { getInputPat  :: Maybe Pat
  , getInputType :: Ty1
  }

type OPat = Pat' () (String, Bool)
data OutputWire = OutputWire
  { getOutputPat  :: Maybe OPat
  , getOutputType :: Ty2
  }

data TypeDecl t x t' x' = TypeDecl String [Ty t x] [Ty t' x']

isProperOPat :: OPat -> Maybe OPat
isProperOPat op = do
  guard (any snd op)
  pure op

-- From a pattern and a list of memory cells, check whether any of the
-- pattern's variables are pointing to one of the memory cell.
mkOPat :: [MemoryCell] -> Pat -> OPat
mkOPat ms = fmap $ \ str -> (str, any ((Just (CellName str) ==) . getCellName) ms)

mkOutputWire :: [MemoryCell] -> Exp -> Ty2 -> OutputWire
mkOutputWire ms e ty = flip OutputWire ty $ do
  p <- exPat e
  isProperOPat (mkOPat ms p) <|> Just ((, False) <$> p)

data Remarkable
  = IsZeroGate
  | IsNotGate
  | IsNandGate
  | IsAndGate
  | IsOrGate
  deriving (Eq)

isRemarkable :: MonadReader CoEnv m => String -> m (Maybe Remarkable)
isRemarkable f = asks (findArr f >=> rmk)

getRemarkable :: MonadReader CoEnv m => Remarkable -> m (Maybe String)
getRemarkable f = do
  arr <- ask
  pure $ getFirst $ flip foldMapArr arr $ \ (k, v) ->
    First $ k <$ guard (rmk v == Just f)

data AllRemarkables ty = AllRemarkables
  { bitTypeName  :: ty
  , zeroGateName :: String
  , notGateName  :: String
  , orGateName   :: String
  , andGateName  :: String
  }

allRemarkables :: CoEnv -> ty -> Maybe (AllRemarkables ty)
allRemarkables env ty = do
  zeroN <- runReader (getRemarkable IsZeroGate) env
  notN  <- runReader (getRemarkable IsNotGate)  env
  orN   <- runReader (getRemarkable IsOrGate)   env
  andN  <- runReader (getRemarkable IsAndGate)  env
  pure (AllRemarkables ty zeroN notN orN andN)

data Compo = Compo
  { monick :: String
  , rmk    :: Maybe Remarkable
  , defn   :: Maybe TypedDef
  , memTys :: [MemoryCell]
  , inpTys :: [InputWire]
  , oupTys :: [OutputWire]
  , stage0 :: [Va] -- memory
           -> [Va] -- stage 0 outputs
  , stage1 :: [Va] -- memory, stage1 inputs
           -> [Va] -- new memory, stage1 outputs
  }

instance Show Compo where
  show _ = "<component>"

fogTy :: Ty t Void -> Ty Unit a
fogTy (Meta x)    = absurd x
fogTy (TVar s t)  = TVar s (fogTy t)
fogTy (Bit _)     = Bit Unit
fogTy (Cable ts)  = Cable (fmap fogTy ts)

splitTy2 :: Ty2 -> ([Ty1], [Ty1])
splitTy2 (Meta x)   = absurd x
splitTy2 (TVar s t) = bimap rewrap rewrap (splitTy2 t) where
  rewrap = fmap (TVar s)
splitTy2 (Bit T0)   = ([Bit Unit], [])
splitTy2 (Bit T1)   = ([], [Bit Unit])
splitTy2 (Cable ts) = ([Cable ts0], [Cable ts1]) where
  (ts0, ts1) = foldMap splitTy2 ts


------------------------------------------------------------------------------
-- monad
------------------------------------------------------------------------------

type MonadCompo s m =
  ( Has CoEnv s
  , MonadState s m
  , MonadWriter (Seq Feedback) m
  )

data TyFailure = TyFailure
  { failureCtx :: Bwd TyClue
  , failureErr :: TyErr
  }

data HasHoles
  = YesHasHoles
  | NoHasNoHoles

instance Semigroup HasHoles where
  NoHasNoHoles <> y = y
  x <> NoHasNoHoles = x
  YesHasHoles <> YesHasHoles = YesHasHoles

instance Monoid HasHoles where
  mempty = NoHasNoHoles

type TyMonad m =
  ( MonadError TyFailure m      -- failure: context and error
  , MonadReader (Bwd TyClue) m  -- what are we in the middle of?
  , MonadWriter HasHoles m      -- have we found holes (in which case stub the def)
  , MonadState TySt m           -- tracking type variables and tasks
  )

------------------------------------------------------------------------------
-- clues
------------------------------------------------------------------------------

data TyClueF t
  = TySOURCE String String      -- declaration, definition
  | TyWIRE String t t           -- wire, got, want
  | TyAPP Compo [Exp]           -- component, args
  | TyCAB [Exp] [t]             -- innards, types
  | TyEXP Exp [t]               -- expression must produce prefix of required
  | TyINPUTS [t] [Pat]          -- check input patterns against types
  | TyOUTPUTS [t] [Exp]         -- check output expressions against types
  | TyEQN Eqn                   -- check an equation
  deriving (Functor, Foldable, Traversable)

type TyClue = TyClueF Typ


------------------------------------------------------------------------------
-- errors
------------------------------------------------------------------------------

data TyErr
  = CableWidth            -- cable widths have mismatched!
  | BitCable              -- a bit has been connected to a cable!
  | CableLoop             -- there's a spatial loop!
  | DecDef String String  -- declaration and definition names mismatch!
  | Stubbed [Feedback]    -- definition already stubbed out!
  | DuplicateWire String  -- same name used for two wires!
  | ConflictingHoles String -- same name used for two holes with different types
  | LongPats              -- too many patterns for the types!
  | ShortPats             -- not enough patterns for the expressions!
  | Don'tKnow String      -- don't know what that component is!
  | Stage0 (Set String)   -- couldn't compute from memory!
  | Stage1 (Set String)   -- couldn't compute from memory and inputs!
  | Junk                  -- spurious extra stuff!
  | BUGSolderMismatch     -- soldering fails to match up properly (my fault)
--  deriving Show

tyErr :: TyMonad m => TyErr -> m x
tyErr e = do
    g <- ask
    g <- traverse (traverse norm) g
    throwError (TyFailure g e)


------------------------------------------------------------------------------
-- state
------------------------------------------------------------------------------

data TySt = TySt
  { tyNew  :: Integer        -- supply of fresh type variables
  , tyDef  :: Arr TyNom Typ  -- store of type definitions
  , wiCxt  :: Cxt            -- per wire defined? type?
  , wiNew  :: Integer        -- supply of fresh wire names
  , coEnv  :: CoEnv          -- known components
  , memTy  :: [[MemoryCell]] -- memories found so far
  , memIn  :: [Pat]          -- memory input patterns
  , memOu  :: [Pat]          -- memory output patterns
  , sched  :: [Task]         -- scheduled tasks so far
  }

data Wire
  = Physical String
  | Holey String
  deriving (Show, Eq, Ord)

type Cxt = Arr Wire (Bool, Typ)

type CoEnv = Arr String Compo
type TyEnv = Arr String TY

type TyNom = Integer

tySt0 :: TySt
tySt0 = TySt
  { tyNew = 0
  , tyDef = emptyArr
  , wiCxt = emptyArr
  , wiNew = 0
  , coEnv = emptyArr
  , memTy = []
  , memIn = []
  , memOu = []
  , sched = []
  }

tyF :: TyMonad m => m Typ
tyF = do
  st <- get
  let u = tyNew st
  put (st {tyNew = u + 1})
  return (Meta u)

wiF :: TyMonad m => m String
wiF = do
  st <- get
  let u = wiNew st
  put (st {wiNew = u + 1})
  return ("|" ++ show u)

tyD :: TyMonad m => (TyNom, Typ) -> m ()
tyD (x, t) = do
  t <- tyO (x ==) t
  st <- get
  put (st {tyDef = insertArr (x, t) (tyDef st)})

tyO :: TyMonad m => (TyNom -> Bool) -> Typ -> m Typ
tyO bad t = do
  t <- hnf t
  case t of
    Meta y | bad y     -> tyErr CableLoop
           | otherwise -> return t
    TVar s t -> pure (TVar s t)
    Bit _ -> pure (Bit Unit)
    Cable ts -> Cable <$> traverse (tyO bad) ts

defineWire :: TyMonad m => Maybe Typ -> Wire -> m Typ
defineWire mt x = do
  g <- gets wiCxt
  case findArr x g of
    Just (False, ty) -> do
      st <- get
      put (st {wiCxt = insertArr (x, (True, ty)) g})
      case mt of
        Nothing -> return ty
        Just ty' -> do
          tyEq (ty, ty')
          return ty'
    Just (True, ty) -> case x of
      Physical nm -> tyErr (DuplicateWire nm)
      Holey{} -> case mt of
        Just ty' -> ty <$ tyEq (ty, ty')
        _ -> pure ty
    Nothing -> do
      ty <- case mt of
        Just ty -> return ty
        _       -> tyF
      st <- get
      put (st {wiCxt = insertArr (x, (True, ty)) g})
      return ty

useWire :: TyMonad m => Wire -> m Typ
useWire x = do
  g <- gets wiCxt
  case findArr x g of
    Just (_, ty) -> return ty
    Nothing -> do
      ty <- tyF
      st <- get
      put (st {wiCxt = insertArr (x, (False, ty)) g})
      return ty

schedule :: TyMonad m => Task -> m ()
schedule ta = do
  st <- get
  put (st {sched = ta : sched st})


------------------------------------------------------------------------------
-- head normalisation
------------------------------------------------------------------------------

class Hnf t where
  hnf :: TyMonad m => t -> m t

instance Hnf Typ where
  hnf (Meta u) = tyvH u where
    tyvH x = do
      g <- gets tyDef
      case findArr x g of
        Nothing -> return (Meta x)
        Just t -> do
          t' <- hnf t
          st <- get
          put (st {tyDef = insertArr (x, t') (tyDef st)})
          return t'
  hnf t       = return t

instance (Hnf s, Hnf t) => Hnf (s, t) where
  hnf (s, t) = (,) <$> hnf s <*> hnf t

instance Hnf t => Hnf [t] where
  hnf = traverse hnf

norm :: TyMonad m => Typ -> m Typ
norm t = hnf t >>= \ t -> case t of
  Cable ts -> Cable <$> traverse norm ts
  _ -> return t

actOnTypedPat :: Applicative f => (ty -> f ty') -> Pat' ty a -> f (Pat' ty' a)
actOnTypedPat f = \case
 PVar ty a  -> PVar <$> f ty <*> pure a
 PCab ty ps -> PCab <$> f ty <*> traverse (actOnTypedPat f) ps

actOnTypedEqn :: Applicative f => (ty -> f ty') -> Eqn' ty -> f (Eqn' ty')
actOnTypedEqn f (ps :=: es)
  = (:=:)
  <$> traverse (actOnTypedPat f) ps
  <*> traverse (traverse f) es

actOnTypedDef :: Applicative f => (ty -> f ty') -> Def' ty -> f (Def' ty')
actOnTypedDef f (Def (fn, ps) es meqns)
  = Def . (fn,)
  <$> traverse (actOnTypedPat f) ps
  <*> traverse (traverse f) es
  <*> traverse (traverse (actOnTypedEqn f)) meqns
actOnTypedDef f (Stub n args) = pure (Stub n args)

normDef :: TyMonad m => TypedDef -> m TypedDef
normDef = actOnTypedDef norm

------------------------------------------------------------------------------
-- unification
------------------------------------------------------------------------------

tyEq :: TyMonad m => (Typ, Typ) -> m ()
tyEq st = hnf st >>= \ st -> case st of
  (Bit _,    Bit _)    -> return ()
  (Cable ss, Cable ts)
    | length ss == length ts -> traverse_ tyEq (zip ss ts)
    | otherwise -> tyErr CableWidth
  (Bit _,    Cable _)  -> tyErr BitCable
  (Cable _,  Bit _)    -> tyErr BitCable
  (Meta x,    t)       -> tyD (x, t)
  (t,        Meta y)   -> tyD (y, t)
  (TVar s t, TVar s' t')
    | s == s' -> return ()
    | otherwise -> tyEq (forget t, forget t')
  (TVar _ t, t') -> tyEq (forget t, t')
  (t, TVar _ t') -> tyEq (t, forget t')


------------------------------------------------------------------------------
-- stubbing
------------------------------------------------------------------------------

stub :: Ty1 -> Va
stub (Meta x)   = absurd x
stub (TVar s t) = stub t
stub (Bit _)    = VQ
stub (Cable ts) = VC (fmap stub ts)
