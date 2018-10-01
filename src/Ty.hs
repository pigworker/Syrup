------------------------------------------------------------------------------
-----                                                                    -----
-----     Ty: types for Syrup                                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Ty where

import Control.Monad
import Control.Monad.State
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

type Ty1 = Ty () Void
type Ty2 = Ty (Ti Void) Void

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

tyErr :: TyErr -> TyM x
tyErr = TyM . const . Left


------------------------------------------------------------------------------
-- state
------------------------------------------------------------------------------

data TySt = TySt
  { tiDag :: Dag TiNom
  } deriving Show

type TiNom = Integer



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

instance MonadState TySt TyM where
  get = TyM $ \ s -> Right (s, s)
  put s = TyM $ \ _ -> Right ((), s)
  