------------------------------------------------------------------------------
-----                                                                    -----
-----     Ty: types for Syrup                                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Language.Syrup.Ty where

import Control.Applicative ((<|>))
import Control.Monad (ap, guard)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.State (MonadState, get, gets, put)

import Data.List (intercalate)
import Data.Void (Void, absurd)

import Language.Syrup.BigArray
import Language.Syrup.Va
import Language.Syrup.Syn
import Language.Syrup.Bwd

------------------------------------------------------------------------------
-- representing Syrup types
------------------------------------------------------------------------------

data Ty t x
  = TyV x
  | Bit t
  | Cable [Ty t x]
  deriving (Eq, Foldable, Traversable)

type Ty1 = Ty () Void
type Ty2 = Ty Ti Void
type Typ = Ty () TyNom

type TypedPat = Pat' Typ String
type TypedExp = Exp' Typ
type TypedEqn = Eqn' Typ
type TypedDef = Def' Typ

data Ti = T0 | T1 deriving (Show, Eq)

sizeTy :: Ty a Void -> Int
sizeTy = \case
  TyV v    -> absurd v
  Bit{}    -> 1
  Cable ts -> 2 + sum (sizeTy <$> ts)

newtype CellName = CellName { cellName :: String }
  deriving (Eq, Show)

data MemoryCell = MemoryCell
  { getCellName :: Maybe CellName
  , getCellType :: Ty1
  } deriving (Show)

data InputWire = InputWire
  { getInputPat  :: Maybe Pat
  , getInputType :: Ty1
  }

type OPat = Pat' () (String, Bool)
data OutputWire = OutputWire
  { getOutputPat  :: Maybe OPat
  , getOutputType :: Ty2
  }

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

fogTy :: Ty t Void -> Ty1
fogTy (TyV x)     = absurd x
fogTy (Bit _)     = Bit ()
fogTy (Cable ts)  = Cable (fmap fogTy ts)

stanTy :: Ty t Void -> Typ
stanTy (TyV x)    = absurd x
stanTy (Bit _)    = Bit ()
stanTy (Cable ts) = Cable (fmap stanTy ts)

splitTy2 :: Ty2 -> ([Ty1], [Ty1])
splitTy2 (Bit T0)   = ([Bit ()], [])
splitTy2 (Bit T1)   = ([], [Bit ()])
splitTy2 (Cable ts) = ([Cable ts0], [Cable ts1]) where
  (ts0, ts1) = foldMap splitTy2 ts


------------------------------------------------------------------------------
-- monad
------------------------------------------------------------------------------

newtype TyM x = TyM
  {tyM :: Bwd TyClue                  -- what are we in the middle of?
       -> TySt                        -- tracking type variables and tasks
       -> Either (Bwd TyClue, TyErr)  -- failure: context and error
                 (x, TySt)            -- success: output and updated state
  }


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
  deriving (Show, Functor, Foldable, Traversable)

type TyClue = TyClueF Typ


------------------------------------------------------------------------------
-- errors
------------------------------------------------------------------------------

data TyErr
  = CableWidth            -- cable widths have mismatched!
  | BitCable              -- a bit has been connected to a cable!
  | CableLoop             -- there's a spatial loop!
  | DecDef String String  -- declaration and definition names mismatch!
  | Stubbed [String]      -- definition already stubbed out!
  | DuplicateWire String  -- same name used for two wires!
  | LongPats              -- too many patterns for the types!
  | ShortPats             -- not enough patterns for the expressions!
  | Don'tKnow String      -- don't know what that component is!
  | Stage0 (Set String)   -- couldn't compute from memory!
  | Stage1 (Set String)   -- couldn't compute from memory and inputs!
  | Junk                  -- spurious extra stuff!
  | BUGSolderMismatch     -- soldering fails to match up properly (my fault)
  deriving Show

tyErr :: TyErr -> TyM x
tyErr e = do
    g <- ask
    g <- traverse (traverse norm) g
    bail (g, e)
  where bail x = TyM $ \ _ _ -> Left x


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
  } deriving Show

type Cxt = Arr String (Bool, Typ)

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

tyF :: TyM Typ
tyF = do
  st <- get
  let u = tyNew st
  put (st {tyNew = u + 1})
  return (TyV u)

wiF :: TyM String
wiF = do
  st <- get
  let u = wiNew st
  put (st {wiNew = u + 1})
  return ("|" ++ show u)

tyD :: (TyNom, Typ) -> TyM ()
tyD (x, t) = do
  t <- tyO (x ==) t
  st <- get
  put (st {tyDef = insertArr (x, t) (tyDef st)})

tyO :: (TyNom -> Bool) -> Typ -> TyM Typ
tyO bad t = do
  t <- hnf t
  case t of
    TyV y | bad y     -> tyErr CableLoop
          | otherwise -> return t
    Bit _ -> pure (Bit ())
    Cable ts -> Cable <$> traverse (tyO bad) ts

defineWire :: Maybe Typ -> String -> TyM Typ
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
    Just (True, _)   -> tyErr (DuplicateWire x)
    Nothing -> do
      ty <- case mt of
        Just ty -> return ty
        _       -> tyF
      st <- get
      put (st {wiCxt = insertArr (x, (True, ty)) g})
      return ty

useWire :: String -> TyM Typ
useWire x = do
  g <- gets wiCxt
  case findArr x g of
    Just (_, ty) -> return ty
    Nothing -> do
      ty <- tyF
      st <- get
      put (st {wiCxt = insertArr (x, (False, ty)) g})
      return ty

schedule :: Task -> TyM ()
schedule ta = do
  st <- get
  put (st {sched = ta : sched st})


------------------------------------------------------------------------------
-- head normalisation
------------------------------------------------------------------------------

class Hnf t where
  hnf :: t -> TyM t

instance Hnf Typ where
  hnf (TyV u) = tyvH u where
    tyvH x = do
      g <- gets tyDef
      case findArr x g of
        Nothing -> return (TyV x)
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

norm :: Typ -> TyM Typ
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

normDef :: TypedDef -> TyM TypedDef
normDef = actOnTypedDef norm

------------------------------------------------------------------------------
-- unification
------------------------------------------------------------------------------

tyEq :: (Typ, Typ) -> TyM ()
tyEq st = hnf st >>= \ st -> case st of
  (Bit _,    Bit _)    -> return ()
  (Cable ss, Cable ts)
    | length ss == length ts -> foldMap tyEq (zip ss ts)
    | otherwise -> tyErr CableWidth
  (Bit _,    Cable _)  -> tyErr BitCable
  (Cable _,  Bit _)    -> tyErr BitCable
  (TyV x,    t)        -> tyD (x, t)
  (t,        TyV y)    -> tyD (y, t)


------------------------------------------------------------------------------
-- stubbing
------------------------------------------------------------------------------

stub :: Ty1 -> Va
stub (Bit _) = VQ
stub (Cable ts) = VC (fmap stub ts)


------------------------------------------------------------------------------
-- boring instances
------------------------------------------------------------------------------

instance Show x => Show (Ty t x) where
  show (TyV x)    = "?" ++ show x
  show (Bit t)    = "<Bit>"
  show (Cable ts) = "[" ++ showTyList ts ++ "]"

showTyList :: Show x => [Ty t x] -> String
showTyList ts = intercalate ", " (fmap show ts)

instance Monad (Ty t) where
  return = TyV
  TyV x    >>= k = k x
  Bit t    >>= _ = Bit t
  Cable ts >>= k = Cable (fmap (>>= k) ts)

instance Applicative (Ty t) where
  pure = return
  (<*>) = ap

instance Functor (Ty t) where
  fmap = ap . return

instance Monad TyM where
  return x = TyM $ \ g s -> Right (x, s)
  TyM af >>= k = TyM $ \ g s -> case af g s of
    Left e -> Left e
    Right (a, s) -> tyM (k a) g s

instance Applicative TyM where
  pure = return
  (<*>) = ap

instance Functor TyM where
  fmap = ap . return

instance Monoid x => Monoid (TyM x) where
  mempty = pure mempty
  mappend = (<>)
instance Semigroup x => Semigroup (TyM x) where
  mx <> my = (<>) <$> mx <*> my

instance MonadState TySt TyM where
  get = TyM $ \ g s -> Right (s, s)
  put s = TyM $ \ g _ -> Right ((), s)

instance MonadReader (Bwd TyClue) TyM where
  ask = TyM $ \ g s -> Right (g, s)
  local f a = TyM $ tyM a . f
