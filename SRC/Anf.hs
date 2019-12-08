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
  } deriving (Functor, Foldable, Traversable)

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

-- If the RHS is already a variable name then we can simply return it.
-- Otherwise we have an arbitrary expression so we introduce a virtual
-- name for it and return an equation connecting this virtual name to
-- the expression.
elabRHS :: Exp -> Fresh (Output, [Eqn])
elabRHS = \case
  Var x -> pure (Output False x, [])
  e     -> do
    vn <- freshVirtualName
    pure (Output True vn, [[PVar vn] :=: [e]])

-- Not much work done here: elaborate the LHS, elaborate the RHS and
-- collect the additional equations added by doing so and finally
-- elaborate all of the equations.
elabDef :: Def -> Fresh (Maybe (String, Gate))
elabDef Stub{} = pure Nothing
elabDef (Def (nm, ps) rhs eqns) = Just <$> do
  let ins = map (Input . elabPat) ps
  os <- mapM elabRHS rhs
  let (ous, oeqns) = unzip os
  lcs <- mapM elabEqn (concat oeqns ++ fromMaybe [] eqns)
  let gate = Gate
       { inputs      = ins
       , outputs     = ous
       , definitions = concat lcs
       }
  pure (nm, gate)

-- When elaborating an equations we have two situations:
--    A,B,C = d,e,f
-- or A,B,C = e
-- The first case can be reduced to solving (A = d, B = e, C = f)
-- The second is bit more complex:
--   - If e is a variable then we're done.
--   - If e is an application then we recursively elaborate all of its
--     arguments as if they were RHS i.e. we want variable names for them
--     and we are ready to pay for it by generating additional equations.
--     Finally we elaborate these additional equations.
elabEqn :: Eqn -> Fresh ([([Output], Expr)])
elabEqn (ps :=: [rhs]) = do
  let ous = map (Output False . elabPat) ps
  case rhs of
    Var x    -> pure [(ous, Alias x)]
    App f es -> do
      aes <- mapM elabRHS es
      let (args, eqs) = unzip aes
      ih <- mapM elabEqn $ concat eqs
      pure $ (ous, Call f (map (Input . outputName) args)) : concat ih
elabEqn (ps :=: rhs) = do
  eqns <- mapM elabEqn (zipWith (\ p e -> [p] :=: [e]) ps rhs)
  pure $ concat eqns

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

foo :: Def
foo =
  Def ("foo", PVar <$> ["A", "B", "C"])
      ([App "and" [Var "A", Var "B"], Var "Z"])
      $ Just [([PVar "Z"] :=: [App "or" [Var "A"
                                        , App "and" [Var "B", Var "C"]]])]

and4 :: Def
and4 = Def ("and4", PVar <$> ["A", "B", "C", "D"])
           [foldr1 (\ a b -> App "and" [a, b]) $ Var <$> ["A", "B", "C", "D"]]
           Nothing

and4' :: Def
and4' =
  Def ("and4'", PVar <$> ["A", "B", "C", "D"])
  [App "and" [ App "and" (Var <$> ["A", "B"])
             , App "and" (Var <$> ["C", "D"])
             ]
  ]
  Nothing

test :: IO ()
test = do
  let runner = print . toANF
  runner foo
  runner and4
  runner and4'
