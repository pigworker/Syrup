------------------------------------------------------------------------------
-----                                                                    -----
-----     Anf: Elaboration to A normal forms                             -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Anf where

import Data.Maybe (fromMaybe)

import Language.Syrup.Fsh
import Language.Syrup.Smp
import Language.Syrup.Syn
import Language.Syrup.Ty

getTyp :: TypedExp -> Typ
getTyp = \case
  Var ty _ -> ty
  Cab ty _ -> ty
  App _ _ -> _a

------------------------------------------------------------------------------
-- Syntax of A normal forms
-- Outputs may be virtual i.e. introduced by the machine but not
-- present in the source code.
-- For instance when we elaborate "f ([X,Y,Z]) = rhs"
-- we obtain "f A = rhs where [X,Y,Z] = A".
-- Here A is a virtual wire and we should not display this name back
-- to users (e.g. if we produce a circuit diagram for their source code).

data Input' nm = Input
  { isVirtualInput :: Bool
  , inputType :: Typ
  , inputDisplayName :: Maybe String
  , inputName :: nm }
  deriving (Functor, Foldable, Traversable)

-- Outputs may be virtual i.e. introduced by the machine but not
-- present in the source code.
-- For instance when we elaborate "A&B&C" we obtain "A&Z where Z = B&C".
-- Here Z is a virtual wire and we should not display this name back
-- to users (e.g. if we produce a circuit diagram for their source code).

data Output' nm = Output
  { isVirtualOutput  :: Bool
  , outputType :: Typ
  , outputDisplayName :: Maybe String
  , outputName :: nm
  } deriving (Functor, Foldable, Traversable)

wire :: Input' nm -> Output' nm
wire (Input v ty dn x) = Output v ty dn x

cowire :: Output' nm -> Input' nm
cowire (Output v ty dn x) = Input v ty dn x

-- Expressions in A normal form: either a variable or a function applied
-- to a bunch of variables. Nothing more complex than that.

data Expr' nm
  = Alias Typ nm
  | Call nm [Input' nm]
  | FanIn [Input' nm]
  | FanOut (Input' nm)
  deriving (Functor, Foldable, Traversable)

type Input  = Input' String
type Output = Output' String
type Expr   = Expr' String

type LetBinding = ([Output], Expr)

data Gate = Gate
  { inputs      :: [Input]
  , outputs     :: [Output]
  , letBindings :: [LetBinding]
  }

-- Needed to merge the successive results.
-- In practice we should *not* have clashing names!
instance Semigroup Gate where
  a <> b = a

------------------------------------------------------------------------------
-- ANF: the Fresh monad to generate fresh names for virtual wires

type ANF = Fresh Int

-- This should return a name not valid in the surface syntax.
freshVirtualName :: ANF String
freshVirtualName = do
  i <- fresh
  pure $ concat ["__VIRTUAL__", show i]

------------------------------------------------------------------------------
-- Elaboration
--
-- If the Exp is already a variable name then we can simply return it.
-- Otherwise we have an arbitrary expression so we introduce a virtual
-- name for it and return a delayed "equation" connecting this virtual
-- name to the expression. We do not reuse the type Eqn because we want
-- to remember that the name on the LHS is virtual.

type Assignment = ([Output], TypedExp)

-- Return an input name for the pattern, a list of inputs
-- corresponding to the names bound in the pattern, and
-- a list of assignments in A-normal form representing
-- the successive fan-outs
elabPat :: TypedPat -> ANF (Input, [Input], [LetBinding])
elabPat p = case p of
  PVar ty x -> let vx = Input False ty Nothing x in pure (vx, [vx], [])
  PCab ty ps -> do
    x <- Input True ty (Just $ show p) <$> freshVirtualName
    ias <- mapM elabPat ps
    let (is, iss, eqnss) = unzip3 ias
    pure (x, concat iss, (wire <$> is, FanOut x) : concat eqnss)

declareAlias :: TypedExp -> ANF (Output, [Assignment])
declareAlias e = do
  vn <- freshVirtualName
  let ty = getTyp e
  let out = Output True ty (show <$> exPat e) vn
  pure (out, [([out], e)])

elabExp :: TypedExp -> ANF (Output, [Assignment])
elabExp = \case
  Var ty x -> pure (Output False ty Nothing x, [])
  e        -> declareAlias e

-- If an expression on the RHS is a variable corresponding to an input
-- wire, we introduce a virtual name for it an do aliasing. This allows
-- us to assume that the named inputs & outputs are always distinct
-- which is a really useful invariant when producing a diagram.

elabRHS :: [Input] -> TypedExp -> ANF (Output, [Assignment])
elabRHS inputs e =
  let dflt = elabExp e
      ins  = map inputName inputs
  in case e of
    Var _ x
      | x `elem` ins -> declareAlias e
      | otherwise    -> dflt
    _ -> dflt

-- Not much work done here: elaborate the LHS, elaborate the RHS and
-- collect the additional equations added by doing so and finally
-- elaborate all of the equations.
elabDef :: TypedDef -> ANF (Maybe (String, Gate))
elabDef Stub{} = pure Nothing
elabDef (Def (nm, ps) rhs eqns) = Just <$> do
  -- obtain
  --  ins:   the definition's inputs
  --  inss:  the definition's bound names on the LHS
  --  lcs0:  the fanouts turning ins into inss
  (ins, inss, lcs0) <- unzip3 <$> mapM elabPat ps
  -- obtain
  --  ous:    the right hand side's outputs
  --  oeqns:  the equations defining the outputs
  (ous, oeqns) <- unzip <$> mapM (elabRHS (concat inss)) rhs
  -- Elaborate the `where`-bound equations
  lcs1 <- mapM elabEqn  (fromMaybe [] eqns)
  -- Elaborate the assignments
  lcs2 <- mapM elabAss  (concat oeqns)
  let gate = Gate
       { inputs      = ins
       , outputs     = ous
       , letBindings = concat (lcs0 ++ lcs1 ++ lcs2)
       }
  pure (nm, gate)

-- When elaborating an equations we have two situations:
--    A,B,C = e
-- or A,B,C = d,e,f
-- The first case can be reduced to the notion of assignment we introduced earlier
-- The second case can be reduced to solving (A = d, B = e, C = f)
elabEqn :: TypedEqn -> ANF [LetBinding]
elabEqn (ps :=: [rhs]) = do
  (is, iss, fanouts) <- unzip3 <$> mapM elabPat ps
  defs <- elabAss (wire <$> is, rhs)
  pure (concat fanouts ++ defs)
elabEqn (ps :=: rhs) = do
  eqns <- mapM elabEqn (zipWith (\ p e -> [p] :=: [e]) ps rhs)
  pure $ concat eqns

-- The assignment A,B,C = e case is bit more complex:
--   - If e is a variable then we're done.
--   - If e is an application then we recursively elaborate all of the
--     expression it has as arguments i.e. we want variable names for them
--     and we are ready to pay for it by generating additional assignments.
--     Finally we elaborate these additional assignments
elabAss :: Assignment -> ANF [LetBinding]
elabAss (ous, e) = case e of
  Var ty x -> pure [(ous, Alias ty x)]
  App f es -> do
    (args, eqs) <- unzip <$> mapM elabExp es
    ih <- mapM elabAss $ concat eqs
    pure $ (ous, Call f (cowire <$> args)) : concat ih
  Cab ty es -> do
    (args, eqs) <- unzip <$> mapM elabExp es
    ih <- mapM elabAss $ concat eqs
    pure $ (ous, FanIn (cowire <$> args)) : concat ih

toGate :: TypedDef -> Maybe (String, Gate)
toGate = evalFresh . elabDef

------------------------------------------------------------------------------
-- Back to Def

-- Erase a Gate back to a Def for pretty-printing purposes.
-- Note that we could also use this function to check that
-- `d` and `toANF d` are bisimilar!
fromGate :: String -> Gate -> TypedDef
fromGate nm g =
  Def (nm, map (\ i -> PVar (inputType i) (inputName i)) (inputs g))
      (map (\ o -> Var (outputType o) (outputName o)) (outputs g))
  $ case letBindings g of
      []   -> Nothing
      eqns -> Just $ map (\ (os, rhs) ->
        (map (\ o -> PVar (outputType o) (outputName o)) os)
                :=:
                [case rhs of
                   Alias ty x -> Var ty x
                   Call f es  -> App f (map (\ i -> Var (inputType i) (inputName i)) es)
                   FanIn os   -> let ty = Cable (map inputType os) in
                                 Cab ty (map (\ i -> Var (inputType i) (inputName i)) os)
                   FanOut i   -> Var (inputType i) (inputName i)
                ])
                     eqns

toANF :: TypedDef -> TypedDef
toANF d = fromMaybe d $ uncurry fromGate <$> toGate d

------------------------------------------------------------------------------
-- Tests

test :: IO ()
test = do
  let runner = print . toANF
  runner foo
  runner and4
  runner and4'
