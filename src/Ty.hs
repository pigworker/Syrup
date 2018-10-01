------------------------------------------------------------------------------
-----                                                                    -----
-----     Ty: types for Syrup                                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE
    MultiParamTypeClasses,
    DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Ty where

import Control.Monad
import Control.Monad.State
import Control.Arrow
import Data.List
import Data.Void
import Data.Monoid
import Data.Maybe

import BigArray
import Dag


------------------------------------------------------------------------------
-- representing Syrup types
------------------------------------------------------------------------------

data Ty t x
  = TyV x
  | Bit t
  | Cable [Ty t x]
  deriving (Foldable, Traversable)

type Ty1 = Ty () Void
type Ty2 = Ty (Ti Void) Void
type Typ = Ty (Ti TiNom) TyNom

data Ti u = T0 | T1 | TiV u

type Tim = Ti TiNom

data CompoTy = CompoTy
  { memTys :: [Ty1]
  , inpTys :: [Ty2]
  , oupTys :: [Ty2]
  } deriving Show


------------------------------------------------------------------------------
-- monad
------------------------------------------------------------------------------

newtype TyM x = TyM {tyM :: TySt -> Either TyErr (x, TySt)}


------------------------------------------------------------------------------
-- errors
------------------------------------------------------------------------------

data TyErr
  = TiLoop
  | CableWidth
  | BitCable
  | CableLoop
  deriving Show

tyErr :: TyErr -> TyM x
tyErr = TyM . const . Left


------------------------------------------------------------------------------
-- state
------------------------------------------------------------------------------

data TySt = TySt
  { tiDag :: Dag TiNom  -- dag of timing constraints
  , tiNew :: Integer    -- supply of fresh time variables
  , tyDag :: Dag TyNom  -- dag of subtyping constraints
  , tyNew :: Integer    -- supply of fresh type variables
  } deriving Show

type TiNom = Integer

type TyNom = Integer

tiF :: TyM Tim
tiF = do
  st <- get
  let u = tiNew st
  put (st {tiNew = u + 1})
  return (TiV u)

tyF :: TyM Typ
tyF = do
  st <- get
  let u = tyNew st
  put (st {tyNew = u + 1})
  return (TyV u)

tySt0 :: TySt
tySt0 = TySt
  { tiDag = Dag emptyArr
  , tiNew = 0
  , tyDag = Dag emptyArr
  , tyNew = 0
  }
  

------------------------------------------------------------------------------
-- subtyping
------------------------------------------------------------------------------

newtype TySb = TySb (Arr TyNom Typ) deriving Show

tySb1 :: (TyNom, Typ) -> TySb
tySb1 = TySb . single

tySb :: TySb -> Typ -> Typ
tySb (TySb g) t = t >>= \ v -> case findArr v g of
  Just s  -> s
  Nothing -> return v

instance Monoid TySb where
  mempty = TySb emptyArr
  mappend (TySb fs) (TySb bs) = TySb
    (appEndo (foldMapArr (\ (u, v) -> Endo $
                          insertArr (u, tySb (TySb bs) v)) fs)
     bs)
instance Semigroup TySb where (<>) = mappend

tyLe :: (Typ, Typ) -> TyM (TiSb, TySb)
tyLe (Bit u,    Bit v)     = (,) <$> tiLe (u, v) <*> pure mempty
tyLe (Cable ss, Cable ts)
  | length ss == length ts = tyLes (zip ss ts)
  | otherwise = tyErr CableWidth
tyLe (Bit _,    Cable _)  = tyErr BitCable
tyLe (Cable _,  Bit _)    = tyErr BitCable
tyLe (TyV x,    TyV y)    = do
  st <- get
  let (xys, tyd) = edge (x, y) (tyDag st)
  put (st {tyDag = tyd})
  return (mempty, TySb (fmap (const (TyV y)) xys))
tyLe (TyV x,    Bit v)    = do
  u <- tiF
  tis <- tiLe (u, v)
  let xt = (x, Bit (tiSb tis u))
  pure (tis, tySb1 xt) <> tyLet xt
tyLe (Bit u,    TyV y)    = do
  v <- tiF
  tis <- tiLe (u, v)
  let yt = (y, Bit (tiSb tis v))
  pure (tis, tySb1 yt) <> tyLet yt
tyLe (TyV x,    Cable ts)
  | any (x ==) (Cable ts) = tyErr CableLoop
  | otherwise = do
  ss <- traverse (const tyF) ts
  (tis, tys) <- tyLe (Cable ss, Cable ts)
  let xt = (x, tySb tys (Cable ss))
  pure (tis, tySb1 xt <> tys) <> tyLet xt
tyLe (Cable ss, TyV y)
  | any (y ==) (Cable ss) = tyErr CableLoop
  | otherwise = do
  ts <- traverse (const tyF) ss
  (tis, tys) <- tyLe (Cable ss, Cable ts)
  let yt = (y, tySb tys (Cable ts))
  pure (tis, tySb1 yt <> tys) <> tyLet yt

tyLet :: (TyNom, Typ) -> TyM (TiSb, TySb)
tyLet (x, t) = do
  st <- get
  let tyd = tyDag st
  let xup = deleteArr x (upSet tyd x)
  let xdn = deleteArr x (downSet tyd x)
  put (st {tyDag = rawDelete (singleton x) tyd})
  foldMapArr (\ (y, ()) -> tyLe (t, TyV y)) xup <>
    foldMapArr (\ (w, ()) -> tyLe (TyV w, t)) xdn

tyLes :: [(Typ, Typ)] -> TyM (TiSb, TySb)
tyLes [] = return mempty
tyLes (c : cs) = do
  (tis, tys) <- tyLe c
  let cs' = (tySb tys *** tySb tys) <$> cs
  (tis', tys') <- tyLes cs'
  return (tis <> tis', tys <> tys')


------------------------------------------------------------------------------
-- subtiming
------------------------------------------------------------------------------

newtype TiSb = TiSb (Arr TiNom Tim) deriving Show

tiSb :: TiSb -> Tim -> Tim
tiSb _  T0             = T0
tiSb _  T1             = T1
tiSb (TiSb g) (TiV u)  = case findArr u g of
  Just t  -> t
  Nothing -> TiV u

instance Monoid TiSb where
  mempty = TiSb emptyArr
  mappend (TiSb fs) (TiSb bs) = TiSb
    (appEndo (foldMapArr (\ (u, v) -> Endo $
                          insertArr (u, tiSb (TiSb bs) v)) fs)
     bs)
instance Semigroup TiSb where (<>) = mappend

tiLe :: (Tim, Tim) -> TyM TiSb
tiLe (T0, _)  = return mempty
tiLe (_, T1)  = return mempty
tiLe (T1, T0) = tyErr TiLoop
tiLe (T1, TiV u) = do
  st <- get
  let (u1s, tid) = upDelete u (tiDag st)
  put (st {tiDag = tid})
  return (TiSb (fmap (const T1) u1s))
tiLe (TiV u, T0) = do
  st <- get
  let (u0s, tid) = downDelete u (tiDag st)
  put (st {tiDag = tid})
  return (TiSb (fmap (const T0) u0s))
tiLe (TiV u, TiV v) = do
  st <- get
  let (uvs, tid) = edge (u, v) (tiDag st)
  put (st {tiDag = tid})
  return (TiSb (fmap (const (TiV v)) uvs))



------------------------------------------------------------------------------
-- boring instances
------------------------------------------------------------------------------

instance (Show t, Show x) => Show (Ty t x) where
  show (TyV x)    = show x
  show (Bit t)    = show t ++ "-Bit"
  show (Cable ts) = "[" ++ intercalate ", " (fmap show ts) ++ "]"

instance Show u => Show (Ti u) where
  show T0      = "0"
  show T1      = "1"
  show (TiV u) = show u

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




------------------------------------------------------------------------------
-- experiments
------------------------------------------------------------------------------

expt1 :: TyM (TiSb, TySb)  -- should succeed, propagating
expt1 = do
  a <- tyF
  b <- tyF
  c <- tyF
  u <- tiF
  tyLe (a, b)
  tyLe (a, c)
  tyLe (b, Bit u)

expt2 :: TyM (TiSb, TySb)
expt2 = do
  a <- tyF
  b <- tyF
  c <- tyF
  d <- tyF
  e <- tyF
  tyLe (a, b)
  tyLe (a, c)
  tyLe (b, Cable [d, e])
  