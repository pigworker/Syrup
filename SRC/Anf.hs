------------------------------------------------------------------------------
-----                                                                    -----
-----     Anf: Elaboration to A normal forms                             -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}

module Syrup.SRC.Anf where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

import Data.Maybe
import Data.Foldable

import Syrup.SRC.Syn
import Syrup.SRC.BigArray

newtype Input' nm = Input { inputName :: nm }
  deriving (Functor, Foldable, Traversable)

data Output' nm = Output
  { outputName    :: nm
  , virtualOutput :: Bool
  } deriving (Functor, Foldable, Traversable)

data Expr' nm
  = Alias nm
  | App nm [Input' nm]
  deriving (Functor, Foldable, Traversable)

type Input  = Input' String
type Output = Output' String
type Expr   = Expr' String

data Gate = Gate
  { virtualGate :: Bool
  , inputs      :: [Input]
  , outputs     :: [Output]
  , definitions :: [([Output], Expr)]
  }

-- Needed to merge the successive results.
-- In practice we should *not* have clashing names!
instance Semigroup Gate where
  a <> b = a

newtype Fresh a = Fresh
  { runFresh :: StateT Int (MaybeT (Writer (Arr String Gate))) a }
  deriving (Functor, Applicative, Monad)

evalFresh :: Fresh a -> (Maybe a, Arr String Gate)
evalFresh = runWriter . runMaybeT . flip evalStateT 0 . runFresh

fresh :: Fresh Int
fresh = Fresh $ do
  i <- get
  put (i + 1)
  pure i

-- This should return a name not valid in the surface syntax.
freshVirtualName :: Fresh String
freshVirtualName = do
  i <- fresh
  pure $ '#' : show i

declare :: String -> Gate -> Fresh ()
declare nm gate = Fresh $ tell (single (nm, gate))

elabPat :: Pat -> Input
elabPat = \case
  PVar x -> Input x
  PCab{} -> error "not supported yet"

elabRHS :: Exp -> Fresh (Output, [Eqn])
elabRHS = \case
  Var x -> pure (Output x False, [])
  e     -> do
    vn <- freshVirtualName
    pure (Output vn True, [[PVar vn] :=: [e]])

elabDef' :: Bool -> Def -> Fresh ()
elabDef' isVirtual Stub{} = undefined
elabDef' isVirtual (Def (nm, ps) rhs eqns) = do
  let ins = map elabPat ps
  os <- mapM elabRHS rhs
  let (ous, oeqns) = unzip os
  lcs <- mapM elabEqn (concat oeqns ++ fromMaybe [] eqns)
  let gate = Gate
       { virtualGate = isVirtual
       , inputs      = ins
       , outputs     = ous
       , definitions = lcs
       }
  declare nm gate

elabDef :: Def -> Fresh ()
elabDef = elabDef' False

elabEqn :: Eqn -> Fresh ([Output], Expr)
elabEqn = undefined
