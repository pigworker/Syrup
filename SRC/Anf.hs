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

newtype Input' nm = Input { inputName :: nm }
  deriving (Functor, Foldable, Traversable)

data Output' nm = Output
  { isVirtual  :: Bool
  , outputName :: nm
  } deriving (Functor, Foldable, Traversable)

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

elabPat :: Pat -> String
elabPat = \case
  PVar x -> x
  PCab{} -> error "not supported yet"

elabRHS :: Exp -> Fresh (Output, [Eqn])
elabRHS = \case
  Var x -> pure (Output False x, [])
  e     -> do
    vn <- freshVirtualName
    pure (Output True vn, [[PVar vn] :=: [e]])

elabDef :: Def -> Fresh (String, Gate)
elabDef Stub{} = undefined
elabDef (Def (nm, ps) rhs eqns) = do
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

toANF :: Def -> Def
toANF d = uncurry fromGate (evalFresh (elabDef d))

test :: IO ()
test = do
  let runner = print . toANF
  runner foo
  runner and4
  runner and4'
