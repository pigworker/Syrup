------------------------------------------------------------------------------
-----                                                                    -----
-----     Ty: types for Syrup                                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE
    MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
    DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Ty where

import Control.Monad
import Control.Monad.State
import Control.Arrow
import Data.List
import Data.Void
import Data.Monoid
import Data.Maybe

import Debug.Trace

import BigArray
import Va
import Syn


------------------------------------------------------------------------------
-- representing Syrup types
------------------------------------------------------------------------------

data Ty t x
  = TyV x
  | Bit t
  | Cable [Ty t x]
  deriving (Foldable, Traversable)

type Ty1 = Ty () Void
type Ty2 = Ty Ti Void
type Typ = Ty () TyNom

data Ti = T0 | T1 deriving Show

data Compo = Compo
  { memTys :: [Ty1]
  , inpTys :: [Ty1]
  , oupTys :: [Ty2]
  , stage0 :: [Va] -- memory
           -> [Va] -- stage 0 outputs
  , stage1 :: [Va] -- memory, stage1 inputs
           -> [Va] -- new memory, stage1 outputs
  }
instance Show Compo where
  show _ = "<component>"

stanTy :: Ty t Void -> Typ
stanTy (TyV x)    = absurd x
stanTy (Bit _)    = Bit ()
stanTy (Cable ts) = Cable (fmap stanTy ts)


------------------------------------------------------------------------------
-- monad
------------------------------------------------------------------------------

newtype TyM x = TyM {tyM :: TySt -> Either TyErr (x, TySt)}


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


------------------------------------------------------------------------------
-- errors
------------------------------------------------------------------------------

data TyErr
  = CableWidth            -- cable widths have mismatched!
  | BitCable              -- a bit has been connected to a cable!
  | CableLoop             -- there's a spatial loop!
  | DecDef String String  -- declaration and definition names mismatch!
  | DuplicateWire String  -- same name used for two wires!
  | LongPats              -- too many patterns for the types!
  | ShortPats             -- not enough patterns for the expressions!
  | Don'tKnow String      -- don't know what that component is!
  | DefLoop               -- circular definition!
  deriving Show

tyErr :: TyErr -> TyM x
tyErr = TyM . const . Left


------------------------------------------------------------------------------
-- state
------------------------------------------------------------------------------

data TySt = TySt
  { tyNew :: Integer        -- supply of fresh type variables
  , tyDef :: Arr TyNom Typ  -- store of type definitions
  , wiCxt :: Cxt            -- per wire defined? type?
  , wiNew :: Integer        -- supply of fresh wire names
  , coEnv :: CoEnv          -- known components
  , memTy :: [Ty1]          -- memory found so far
  , memIn :: [Pat]          -- memory input patterns
  , memOu :: [Pat]          -- memory output patterns
  , sched :: [Task]         -- scheduled tasks so far
  } deriving Show

type Cxt = Arr String (Bool, Typ)

type CoEnv = Arr String Compo

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
  trace ("tyF" ++ show u) $ return (TyV u)

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
useWire x = trace ("useWire " ++ x) $ do
  g <- gets wiCxt
  case findArr x g of
    Just (_, ty) -> trace "already" $ return ty
    Nothing -> do
      ty <- tyF
      st <- get
      put (st {wiCxt = insertArr (x, (False, ty)) g})
      trace ("fresh" ++ show ty) $ return ty

schedule :: Task -> TyM ()
schedule ta = do
  st <- get
  put (st {sched = ta : sched st})


------------------------------------------------------------------------------
-- unification
------------------------------------------------------------------------------

tyEq :: (Typ, Typ) -> TyM ()
tyEq st = hnf st >>= \ st -> case trace (show st) st of
  (Bit _,    Bit _)    -> return ()
  (Cable ss, Cable ts)
    | length ss == length ts -> foldMap tyEq (zip ss ts)
    | otherwise -> tyErr CableWidth
  (Bit _,    Cable _)  -> tyErr BitCable
  (Cable _,  Bit _)    -> tyErr BitCable
  (TyV x,    t)        -> tyD (x, t)
  (t,        TyV y)    -> tyD (y, t)


------------------------------------------------------------------------------
-- boring instances
------------------------------------------------------------------------------

instance (Show t, Show x) => Show (Ty t x) where
  show (TyV x)    = "?" ++ show x
  show (Bit t)    = show t ++ "-Bit"
  show (Cable ts) = "[" ++ intercalate ", " (fmap show ts) ++ "]"

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
  return x = TyM $ \ s -> Right (x, s)
  TyM af >>= k = TyM $ \ s -> case af s of
    Left e -> Left e
    Right (a, s) -> tyM (k a) s

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
  get = TyM $ \ s -> Right (s, s)
  put s = TyM $ \ _ -> Right ((), s)


