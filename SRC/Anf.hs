------------------------------------------------------------------------------
-----                                                                    -----
-----     Anf: Elaboration to A normal forms                             -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Syrup.SRC.Anf where

import Control.Monad.State
import Data.Maybe

import Syrup.SRC.Syn
import Syrup.SRC.Smp

------------------------------------------------------------------------------
-- Syntax of A normal forms

newtype Input' nm = Input { inputName :: nm }
  deriving (Functor, Foldable, Traversable)

-- Outputs may be virtual i.e. introduced by the machine but not
-- present in the source code.
-- For instance when we elaborate "A&B&C" we obtain "A&Z where Z = B&C".
-- Here Z is a virtual wire and we should not display this name back
-- to users (e.g. if we produce a circuit diagram for their source code).

data Output' nm = Output
  { isVirtual  :: Bool
  , outputName :: nm
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

-- Expressions in A normal form: either a variable or a function applied
-- to a bunch of variables. Nothing more complex than that.

data Expr' nm
  = Alias nm
  | Call nm [Input' nm]
  deriving (Functor, Foldable, Traversable)

type Input  = Input' String
type Output = Output' String
type Expr   = Expr' String

data Gate = Gate
  { inputs      :: [Input]
  , outputs     :: [Output]
  , definitions :: [([Output], Expr)]
  }

-- Needed to merge the successive results.
-- In practice we should *not* have clashing names!
instance Semigroup Gate where
  a <> b = a

------------------------------------------------------------------------------
-- Fresh monad: generate fresh names for virtual wires

newtype Fresh a = Fresh
  { runFresh :: State Int a }
  deriving (Functor, Applicative, Monad)

evalFresh :: Fresh a -> a
evalFresh = flip evalState 0 . runFresh

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

------------------------------------------------------------------------------
-- Elaboration

elabPat :: Pat -> String
elabPat = \case
  PVar x -> x
  PCab{} -> error "not supported yet"

-- If the Exp is already a variable name then we can simply return it.
-- Otherwise we have an arbitrary expression so we introduce a virtual
-- name for it and return a delayed "equation" connecting this virtual
-- name to the expression. We do not reuse the type Eqn because we want
-- to remember that the name on the LHS is virtual.
type Assignment = ([Output], Exp)

declareAlias :: Exp -> Fresh (Output, [Assignment])
declareAlias e = do
  vn <- freshVirtualName
  let out = Output True vn
  pure (out, [([out], e)])

elabExp :: Exp -> Fresh (Output, [Assignment])
elabExp = \case
  Var x -> pure (Output False x, [])
  e     -> declareAlias e

-- If an expression on the RHS is a variable corresponding to an input
-- wire, we introduce a virtual name for it an do aliasing. This allows
-- us to assume that the named inputs & outputs are always distinct
-- which a really useful invariant when producing a diagram.

elabRHS :: [Input] -> Exp -> Fresh (Output, [Assignment])
elabRHS inputs e =
  let dflt = elabExp e
      ins  = map inputName inputs
  in case e of
    Var x | x `elem` ins -> declareAlias e
          | otherwise    -> dflt
    _ -> dflt

-- Not much work done here: elaborate the LHS, elaborate the RHS and
-- collect the additional equations added by doing so and finally
-- elaborate all of the equations.
elabDef :: Def -> Fresh (Maybe (String, Gate))
elabDef Stub{} = pure Nothing
elabDef (Def (nm, ps) rhs eqns) = Just <$> do
  let ins = map (Input . elabPat) ps
  os <- mapM (elabRHS ins) rhs
  let (ous, oeqns) = unzip os
  lcs1 <- mapM elabEqn  (fromMaybe [] eqns)
  lcs2 <- mapM elabAss  (concat oeqns)
  let gate = Gate
       { inputs      = ins
       , outputs     = ous
       , definitions = concat (lcs1 ++ lcs2)
       }
  pure (nm, gate)

-- When elaborating an equations we have two situations:
--    A,B,C = e
-- or A,B,C = d,e,f
-- The first case can be reduced to the notion of assignment we introduced earlier
-- The second case can be reduced to solving (A = d, B = e, C = f)
elabEqn :: Eqn -> Fresh ([([Output], Expr)])
elabEqn (ps :=: [rhs]) = do
  let ous = map (Output False . elabPat) ps
  elabAss (ous, rhs)
elabEqn (ps :=: rhs) = do
  eqns <- mapM elabEqn (zipWith (\ p e -> [p] :=: [e]) ps rhs)
  pure $ concat eqns

-- The assignment case is bit more complex:
--   - If e is a variable then we're done.
--   - If e is an application then we recursively elaborate all of its
--     the expression it has as arguments i.e. we want variable names for them
--     and we are ready to pay for it by generating additional assignments.
--     Finally we elaborate these additional assignments

elabAss :: Assignment -> Fresh ([([Output], Expr)])
elabAss (ous, e) = case e of
  Var x    -> pure [(ous, Alias x)]
  App f es -> do
    aes <- mapM elabExp es
    let (args, eqs) = unzip aes
    ih <- mapM elabAss $ concat eqs
    pure $ (ous, Call f (map (Input . outputName) args)) : concat ih

toGate :: Def -> Maybe (String, Gate)
toGate = evalFresh . elabDef

------------------------------------------------------------------------------
-- Back to Def

-- Erase a Gate back to a Def for pretty-printing purposes.
-- Not that we could also use this function to check that
-- `d` and `toANF d` are bisimilar!
fromGate :: String -> Gate -> Def
fromGate nm g =
  Def (nm, map (PVar . inputName) (inputs g))
      (map (Var . outputName) (outputs g))
  $ case definitions g of
      []   -> Nothing
      eqns -> Just $ map (\ (os, rhs) ->
                           (map (PVar . outputName) os)
                           :=:
                           [case rhs of
                              Alias x   -> Var x
                              Call f es -> App f (map (Var . inputName) es)
                           ])
                     eqns

toANF :: Def -> Def
toANF d = fromMaybe d $ uncurry fromGate <$> toGate d

------------------------------------------------------------------------------
-- Tests

test :: IO ()
test = do
  let runner = print . toANF
  runner foo
  runner and4
  runner and4'
