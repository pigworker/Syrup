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

data Compo = Compo
  { memTys :: [Ty1]
  , inpTys :: [Ty2]
  , oupTys :: [Ty2]
  } deriving Show

stanTy1 :: Ty1 -> Typ
stanTy1 (TyV x)    = absurd x
stanTy1 (Bit ())   = Bit T0
stanTy1 (Cable ts) = Cable (fmap stanTy1 ts)

stanTy2 :: Tim -> Ty2 -> Typ
stanTy2 v (TyV x)       = absurd x
stanTy2 v (Bit (TiV u)) = absurd u
stanTy2 v (Bit T1)      = Bit v
stanTy2 v (Bit T0)      = Bit T0


------------------------------------------------------------------------------
-- monad
------------------------------------------------------------------------------

newtype TyM x = TyM {tyM :: TySt -> Either TyErr (x, TySt)}


------------------------------------------------------------------------------
-- head normalisation
------------------------------------------------------------------------------

class Hnf t where
  hnf :: t -> TyM t

instance Hnf Tim where
  hnf (TiV u) = tivH u where
    tivH u = do
      st <- get
      let g = tiDef st
      case findArr u g of  -- BigArray needs monadic update
        Nothing -> return (TiV u)
        Just t -> do
          t' <- hnf t
          put (st {tiDef = insertArr (u, t') g})
          return t'
  hnf t       = return t

instance Hnf Typ where
  hnf (TyV u) = tyvH u where
    tyvH x = do
      st <- get
      let g = tyDef st
      case findArr x g of
        Nothing -> return (TyV x)
        Just t -> do
          t' <- hnf t
          put (st {tyDef = insertArr (x, t') g})
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
  = TiLoop                -- there's a timeloop!
  | CableWidth            -- cable widths have mismatched!
  | BitCable              -- a bit has been connected to a cable!
  | CableLoop             -- there's a spatial loop!
  | DecDef String String  -- declaration and definition names mismatch!
  | DuplicateWire String  -- same name used for two wires!
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
  { tiDag :: Dag TiNom      -- dag of timing constraints
  , tiNew :: Integer        -- supply of fresh time variables
  , tiDef :: Arr TiNom Tim  -- store of time definitions
  , tyDag :: Dag TyNom      -- dag of subtyping constraints
  , tyNew :: Integer        -- supply of fresh type variables
  , tyDef :: Arr TyNom Typ  -- store of type definitions
  , wiCxt :: Cxt            -- per wire defined? type?
  , wiNew :: Integer        -- supply of fresh wire names
  , coEnv :: CoEnv          -- known components
  } deriving Show

type Cxt = Arr String (Bool, Typ)

type CoEnv = Arr String Compo

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

wiF :: TyM String
wiF = do
  st <- get
  let u = wiNew st
  put (st {wiNew = u + 1})
  return ("|" ++ show u)

tySt0 :: TySt
tySt0 = TySt
  { tiDag = Dag emptyArr
  , tiNew = 0
  , tiDef = emptyArr
  , tyDag = Dag emptyArr
  , tyNew = 0
  , tyDef = emptyArr
  , wiCxt = emptyArr
  , wiNew = 0
  , coEnv = emptyArr
  }

tiD :: (TiNom, Tim) -> TyM ()
tiD (u, t) = do
  t <- tiO u t
  st <- get
  put (st {tiDef = insertArr (u, t) (tiDef st)})

tiO :: TiNom -> Tim -> TyM Tim
tiO u t = do
  t <- hnf t
  case t of
    TiV v | u == v -> tyErr DefLoop
    _ -> return t

tyD :: (TyNom, Typ) -> TyM ()
tyD (x, t) = do
  t <- tyO (x ==) t  -- is this check strong enough?
  st <- get
  put (st {tyDef = insertArr (x, t) (tyDef st)})

tyO :: (TyNom -> Bool) -> Typ -> TyM Typ
tyO bad t = do
  t <- hnf t
  case t of
    TyV y | bad y     -> tyErr DefLoop
          | otherwise -> return t
    Bit u -> Bit <$> hnf u
    Cable ts -> Cable <$> traverse (tyO bad) ts

defineWire :: String -> TyM Typ
defineWire x = do
  st <- get
  let g = wiCxt st
  case findArr x g of
    Just (False, ty) -> do
      put (st {wiCxt = insertArr (x, (True, ty)) g})
      return ty
    Just (True, _)   -> tyErr (DuplicateWire x)
    Nothing -> do
      ty <- tyF
      put (st {wiCxt = insertArr (x, (True, ty)) g})
      return ty

useWire :: String -> TyM Typ
useWire x = do
  st <- get
  let g = wiCxt st
  case findArr x g of
    Just (_, ty) -> return ty
    Nothing -> do
      ty <- tyF
      put (st {wiCxt = insertArr (x, (False, ty)) g})
      return ty


------------------------------------------------------------------------------
-- subtyping
------------------------------------------------------------------------------

tyLe :: (Typ, Typ) -> TyM ()
tyLe st = hnf st >>= \ st -> case st of
  (Bit u,    Bit v)     -> tiLe (u, v)
  (Cable ss, Cable ts)
    | length ss == length ts -> foldMap tyLe (zip ss ts)
    | otherwise -> tyErr CableWidth
  (Bit _,    Cable _)  -> tyErr BitCable
  (Cable _,  Bit _)    -> tyErr BitCable
  (TyV x,    TyV y) | x == y -> return ()
                    | otherwise -> do
    st <- get
    let (xys, tyd) = edge (x, y) (tyDag st)
    put (st {tyDag = tyd})
    foldMapSet (\ x -> tyD (x, TyV y)) xys
  (TyV x,    Bit v)    -> do
    u <- tiF
    tiLe (u, v)
    tyLet (x, Bit u)
  (Bit u,    TyV y)    -> do
    v <- tiF
    tiLe (u, v)
    tyLet (y, Bit v)
  (TyV x,    Cable ts) -> do
    xc <- gets (dagClosure x . tyDag)
    ts <- traverse (tyO (`inSet` xc)) ts
    ss <- traverse (const tyF) ts
    tyLe (Cable ss, Cable ts)
    tyLet (x, Cable ss)
  (Cable ss, TyV y) -> do
    yc <- gets (dagClosure y . tyDag)
    ss <- traverse (tyO (`inSet` yc)) ss
    ts <- traverse (const tyF) ss
    tyLe (Cable ss, Cable ts)
    tyLet (y, Cable ts)
  where
    tyLet (x, t) = do
      tyD (x, t)
      st <- get
      let tyd = tyDag st
      let xup = deleteArr x (upSet tyd x)
      let xdn = deleteArr x (downSet tyd x)
      put (st {tyDag = rawDelete (singleton x) tyd})
      foldMapSet (\ y -> tyLe (t, TyV y)) xup
      foldMapSet (\ w -> tyLe (TyV w, t)) xdn


------------------------------------------------------------------------------
-- subtiming
------------------------------------------------------------------------------

tiLe :: (Tim, Tim) -> TyM ()
tiLe st = hnf st >>= \ st -> case st of
  (T0, _)  -> return ()
  (_, T1)  -> return ()
  (T1, T0) -> tyErr TiLoop
  (T1, TiV u) -> do
    st <- get
    let (u1s, tid) = upDelete u (tiDag st)
    put (st {tiDag = tid})
    foldMapSet (\ u -> tiD (u, T1)) u1s
  (TiV u, T0) -> do
    st <- get
    let (u0s, tid) = downDelete u (tiDag st)
    put (st {tiDag = tid})
    foldMapSet (\ u -> tiD (u, T0)) u0s
  (TiV u, TiV v) -> do
    st <- get
    let (uvs, tid) = edge (u, v) (tiDag st)
    put (st {tiDag = tid})
    foldMapSet (\ u -> tiD (u, TiV v)) uvs


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

expt1 :: TyM ()  -- should succeed, propagating
expt1 = do
  a <- tyF
  b <- tyF
  c <- tyF
  u <- tiF
  tyLe (a, b)
  tyLe (a, c)
  tyLe (b, Bit u)

expt2 :: TyM ()
expt2 = do
  a <- tyF
  b <- tyF
  c <- tyF
  d <- tyF
  e <- tyF
  tyLe (a, b)
  tyLe (a, c)
  tyLe (b, Cable [d, e])

expt3 :: TyM () -- should fail but it DOESN'T!
expt3 = do
  a <- tyF
  b <- tyF
  tyLe (a, b)
  tyLe (b, Cable [a])
