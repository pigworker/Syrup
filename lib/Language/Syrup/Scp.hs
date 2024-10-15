------------------------------------------------------------------------------
-----                                                                    -----
-----     Scp: Scopechecking Syrup                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Syrup.Scp where

import Control.Monad (foldM, unless, when)

import Data.Bifunctor ()
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.List (intersperse)
import Data.Monoid ()

import Language.Syrup.BigArray
import Language.Syrup.Fdk
import Language.Syrup.Syn

data Scope = Scope
  { global :: Names
  , local  :: Names
  }

emptyScope :: Scope
emptyScope = Scope emptyArr emptyArr

globalScope :: Names -> Scope
globalScope gl = Scope gl emptyArr

newtype Extension (l :: ScopeLevel) = Extend { getExtension :: Names }

emptyExtension :: Extension l
emptyExtension = Extend emptyArr

newtype ScopeM a = ScopeM { runScopeM :: ([ScopeError], a) }
  deriving (Functor, Applicative, Monad)

scopeError :: ScopeError -> ScopeM ()
scopeError err = ScopeM ([err], ())

mergeLocalScope :: Names -> Names -> ScopeM Names
mergeLocalScope lc1 lc2 = do
  let i = intersectSet lc1 lc2
  unless (isEmptyArr i) $ scopeError (Shadowing Local i)
  pure $ lc1 <> lc2

mergeScope :: Scope -> Scope -> ScopeM Scope
mergeScope (Scope gl1 lc1) (Scope gl2 lc2) =
  Scope (gl1 <> gl2) <$> mergeLocalScope lc1 lc2

mergeExtension :: Extension l -> Extension l -> ScopeM (Extension l)
mergeExtension (Extend e1) (Extend e2) = Extend <$> mergeLocalScope e1 e2

class KnownLevel l where
  extend     :: Scope -> Extension l -> Scope
  declareVar :: Scope -> Name -> ScopeM (Extension l)

instance KnownLevel 'Local where
  extend (Scope gl lc) (Extend lce) = Scope gl (lc <> lce)
  declareVar ga nm = do
    let lc = local ga
    let e  = singleton nm
    if (nm `inSet` lc)
      then emptyExtension <$ scopeError (Shadowing Local e)
      else pure (Extend e)

instance KnownLevel 'Global where
  extend (Scope gl lc) (Extend gle) = Scope (gl <> gle) lc
  declareVar ga nm = do
    let gc = global ga
    let e  = singleton nm
    Extend e <$ when (nm `inSet` gc) (scopeError (Shadowing Global e))

hints :: Names -> Name -> Names
hints ga nm = foldMapSet keep ga where

  check = toLower <$> nm

  keep :: Name -> Names
  keep cnd | map toLower cnd == check = singleton cnd
           | otherwise                = emptyArr

isLocalVar :: Scope -> Name -> ScopeM ()
isLocalVar ga nm = do
  let lc = local ga
  unless (nm `inSet` lc) $
    scopeError $ OutOfScope Local nm (hints lc nm)

isGlobalVar :: Scope -> Name -> ScopeM ()
isGlobalVar ga nm = do
  let gc = global ga
  unless (nm `inSet` gc) $
    scopeError $ OutOfScope Global nm (hints gc nm)

type family Level t :: ScopeLevel where
  Level [a]         = Level a
  Level (Maybe a)   = Level a
  Level Pat         = 'Local
  Level Eqn         = 'Local
  Level (DEC' a)    = 'Global
  Level (Source' a) = 'Global

class Scoped t where
  scopecheck :: Scope                         -- input scope
             -> t                             -- term to analise
             -> ScopeM (Extension (Level t))  -- error or scope extension

  -- we do not define the instance `Scoped a => Scoped [a]` because
  -- lists of scoped things can have so many different meanings.
  -- Here we define simultaneous binding (for patterns) but we may
  -- also want:
  --   mutual bindings for lists of equations
  --   telescopic bindings for lists of declarations

  default scopecheck
    :: (t ~ f a, Traversable f, Scoped a, Level t ~ Level a)
    => Scope -> t -> ScopeM (Extension (Level t))
  scopecheck ga ts = do
    es <- mapM (scopecheck ga) ts
    foldM mergeExtension emptyExtension es

instance Scoped a => Scoped (Maybe a)

instance Scoped Pat where
  scopecheck ga = \case
    PVar _ x  -> declareVar ga x
    PCab _ ps -> scopecheck ga ps

instance Scoped Exp where
  scopecheck ga = \case
    Var _ s  -> emptyExtension <$ isLocalVar ga s
    Hol _ s  -> pure emptyExtension
    App _ f es -> do
      isGlobalVar ga f
      scopecheck ga es
    Cab _ es -> scopecheck ga es

instance Scoped [Pat]
instance Scoped [Exp]

instance Scoped [Eqn] where
  scopecheck ga whr = do
    -- all of these are mutually defined so we must check the
    -- body of the equations after having bound the declared
    -- variables
    let (ps, ts) = foldMap (\ (ps :=: ts) -> (ps, ts)) whr
    e <- scopecheck ga ps
    e <$ scopecheck (ga `extend` e) ts

instance Scoped Def where
  scopecheck ga (Def (nm, lhs) rhs whr) = do
    isGlobalVar ga nm
    -- bind patterns in the lhs
    elhs <- scopecheck ga lhs
    let ga' = ga `extend` elhs
    -- bind patterns in the where clauses
    ewhr <- scopecheck ga' whr
    let ga'' = ga' `extend` ewhr
    -- check rhs in the extended scope
    emptyExtension <$ scopecheck ga'' rhs
  scopecheck ga Stub{} = pure emptyExtension

instance Scoped (DEC' a) where
  scopecheck ga (DEC (nm, _) _) = declareVar ga nm

instance Scoped EXPT where
  scopecheck ga = \case
    Tabulate nm         -> emptyExtension <$ isGlobalVar ga nm
    Simulate nm _ _     -> emptyExtension <$ isGlobalVar ga nm
    Bisimilarity nm nm' -> do
      isGlobalVar ga nm
      isGlobalVar ga nm'
      pure emptyExtension
    Display nm -> do
      isGlobalVar ga nm
      pure emptyExtension
    Dnf nm -> do
      isGlobalVar ga nm
      pure emptyExtension
    Print nm -> do
      isGlobalVar ga nm
      pure emptyExtension
    Typing nm -> do
      isGlobalVar ga nm
      pure emptyExtension
    Anf nm -> do
      isGlobalVar ga nm
      pure emptyExtension
    Costing nms nm -> do
      traverse_ (isGlobalVar ga) nms
      isGlobalVar ga nm
      pure emptyExtension
    Simplify nm -> do
      isGlobalVar ga nm
      pure emptyExtension

instance Scoped (Source' a) where
  scopecheck ga = \case
    Declaration d -> scopecheck ga d
    TypeAlias{}   -> emptyExtension <$ pure ()
    Definition d  -> emptyExtension <$ scopecheck ga d
    Experiment e  -> emptyExtension <$ scopecheck ga e

stub :: (Source' a, String) -> [Feedback]
     -> [Either Feedback (Source' a, String)]
     -> [Either Feedback (Source' a, String)]
stub (Definition (Def (nm, _) _ _), src) msg rst
   = Right (Definition (Stub nm msg), src) : rst
stub _ msg rst = map Left msg ++ rst

check :: Scope -> [Either Feedback (Source' a, String)] -> [Either Feedback (Source' a, String)]
check ga []                     = []
check ga (Left err        : ds) = Left err : check ga ds
check ga (Right (d , src) : ds) = do
  let (errs, e) = runScopeM (scopecheck ga d)
  let ga'       = ga `extend` e

  let status    = foldMap categorise errs
  let msg       = AScopeError <$> errs
  case isErroring status of
    True  -> stub (d, src) msg (check ga' ds)
    False -> map Left msg ++ Right (d, src) : check ga' ds
