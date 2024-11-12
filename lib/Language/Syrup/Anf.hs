------------------------------------------------------------------------------
-----                                                                    -----
-----     Anf: Elaboration to A normal forms                             -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Anf where

import Control.Monad.State

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..), Sum(..))
import Data.Traversable (for)

import Language.Syrup.BigArray
import Language.Syrup.Fsh
import Language.Syrup.Pretty (basicShow)
import Language.Syrup.Smp
import Language.Syrup.Syn
import Language.Syrup.Ty

getTyp :: TypedExp -> Typ
getTyp = \case
  Var ty _ -> ty
  Cab ty _ -> ty
  App (ty :| _) _ _ -> ty
  e -> Bit Unit -- default value :(

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
  | Call (NonEmpty Typ) nm [Input' nm]
  | Copy Typ (Input' nm)
  | FanIn [Input' nm]
  | FanOut (Input' nm)
  deriving (Functor, Foldable, Traversable)

type Input  = Input' String
type Output = Output' String
type Expr   = Expr' String

type LetBinding = (NonEmpty Output, Expr)

data Gate = Gate
  { inputs      :: [Input]
  , outputs     :: (NonEmpty Output)
  , letBindings :: [LetBinding]
  }

-- Needed to merge the successive results.
-- In practice we should *not* have clashing names!
instance Semigroup Gate where
  a <> b = a

------------------------------------------------------------------------------
-- ANF: the Fresh monad to generate fresh names for virtual wires

type ANF = StateT (Arr String (NonEmpty Output)) (Fresh Int)

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

type Assignment = (NonEmpty Output, TypedExp)

-- Return an input name for the pattern, a list of inputs
-- corresponding to the names bound in the pattern, and
-- a list of assignments in A-normal form representing
-- the successive fan-outs
elabPat :: TypedPat -> ANF (Input, ([Input], [LetBinding]))
elabPat p = case p of
  PVar ty x -> let vx = Input False ty Nothing x in pure (vx, ([vx], []))
  PCab ty (p : ps) -> do
    x <- Input True ty (Just $ basicShow p) <$> freshVirtualName
    res0 <- mapM elabPat (p :| ps)
    let (is, res1) = NonEmpty.unzip res0
    let (iss, eqnss) = NonEmpty.unzip res1
    pure (x, (concatMap toList iss, (wire <$> is, FanOut x) : concat eqnss))

declareAlias :: TypedExp -> ANF (Output, [Assignment])
declareAlias e = do
  vn <- freshVirtualName
  let ty = getTyp e
  let out = Output True ty (basicShow <$> exPat e) vn
  pure (out, [(out :| [], e)])

elabExp :: TypedExp -> ANF (Output, [Assignment])
elabExp = \case
  Var ty x -> do
    x' <- elabVar x
    let isVirtual = x /= x' -- we got a name from a copy box back!
    pure (Output isVirtual ty (Just x) x', [])
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
--      | x `elem` ins -> declareAlias e
      | otherwise    -> dflt
    _ -> dflt


-- If a wire has more than 2 ends (1 input as enforced by
-- scope checking and 2+ outputs), then introduce one virtual
-- name for each output and add a copy box taking the input
-- and producing all the virtual outputs.
-- When we will elaborate a (Var ty x), we will check whether
-- we need to use one of the virtual names thus introduced.
declareCopies :: (String, (First Typ, Sum Int)) -> ANF [LetBinding]
declareCopies (x, (First (Just ty), Sum n))
  | n <= 2 = pure [] -- there are two ends to each cable
  | otherwise = do
      os <- for (2 :| [3..n]) $ const $ do
              nm <- freshVirtualName
              pure (Output True ty (Just x) nm)
      -- we store these names in the ANF monad for use in elabVar
      modify (insertArr (x, os))
      pure [(os, Copy ty (Input False ty Nothing x))]

-- Not much work done here: elaborate the LHS, elaborate the RHS and
-- collect the additional equations added by doing so and finally
-- elaborate all of the equations.
elabDef :: TypedDef -> ANF (Maybe (String, Gate))
elabDef Stub{} = pure Nothing
elabDef def@(Def (nm, ps) rhs eqns) = do
  lcsA <- foldMapArr snd <$> travArr declareCopies (allVars def)
  -- obtain
  --  ins:   the definition's inputs
  --  inss:  the definition's bound names on the LHS
  --  lcs0:  the fanouts turning ins into inss
  res0 <- mapM elabPat ps
  let (ins, res1) = NonEmpty.unzip res0
  let (inss, lcs0) = NonEmpty.unzip res1
  -- obtain
  --  ous:    the right hand side's outputs
  --  oeqns:  the equations defining the outputs
  (ous, oeqns) <- NonEmpty.unzip <$> mapM (elabRHS (concat inss)) rhs
  -- Elaborate the `where`-bound equations
  lcs1 <- mapM elabEqn  (fromEqns eqns)
  -- Elaborate the assignments
  lcs2 <- mapM elabAss  (concat oeqns)
  let gate = Gate
       { inputs      = ins
       , outputs     = ous
       , letBindings = concat (lcsA : lcs0 ++ lcs1 ++ lcs2)
       }
  pure (Just (nm, gate))

-- When elaborating an equations we have two situations:
--    A,B,C = e
-- or A,B,C = d,e,f
-- The first case can be reduced to the notion of assignment we introduced earlier
-- The second case can be reduced to solving (A = d, B = e, C = f)
elabEqn :: TypedEqn -> ANF [LetBinding]
elabEqn (ps :=: (rhs :| [])) = do
  res0 <- mapM elabPat ps
  let (is, res1) = NonEmpty.unzip res0
  let (iss, fanouts) = NonEmpty.unzip res1
  defs <- elabAss (wire <$> is, rhs)
  pure (concat fanouts ++ defs)
elabEqn (ps :=: rhs) = do
  eqns <- mapM elabEqn (NonEmpty.zipWith (\ p e -> (p :| []) :=: (e :| [])) ps rhs)
  pure $ concat eqns

-- If that end of the wire is used more than once, we
-- need to use one of the virtual names coming out of
-- the associated copy box instead. This is what this
-- does, being careful to *not* put the consumed name
-- back in the state.
elabVar :: String -> ANF String
elabVar x = gets (findArr x) >>= \case
  Nothing -> pure x
  Just (n :| []) -> pure x -- IMPOSSIBLE?
  Just (n :| m : ns) -> do
    modify (insertArr (x, m :| ns))
    pure (outputName n)

-- The assignment A,B,C = e case is bit more complex:
--   - If e is a variable then we're done.
--   - If e is an application then we recursively elaborate all of the
--     expression it has as arguments i.e. we want variable names for them
--     and we are ready to pay for it by generating additional assignments.
--     Finally we elaborate these additional assignments
elabAss :: Assignment -> ANF [LetBinding]
elabAss (ous, e) = case e of
  Var ty x -> pure . (ous,) . Alias ty <$> elabVar x
  App tys f es -> do
    (args, eqs) <- unzip <$> mapM elabExp es
    ih <- mapM elabAss $ concat eqs
    pure $ (ous, Call tys f (cowire <$> args)) : concat ih
  Cab ty es -> do
    (args, eqs) <- unzip <$> mapM elabExp es
    ih <- mapM elabAss $ concat eqs
    pure $ (ous, FanIn (cowire <$> args)) : concat ih

toGate :: TypedDef -> Maybe (String, Gate)
toGate = evalFresh . (`evalStateT` emptyArr) . elabDef

------------------------------------------------------------------------------
-- Back to Def

-- Erase a Gate back to a Def for pretty-printing purposes.
-- Note that we could also use this function to check that
-- `d` and `toANF d` are bisimilar!
fromGate :: String -> Gate -> TypedDef
fromGate nm g =
  Def (nm, map (\ i -> PVar (inputType i) (inputName i)) (inputs g))
      (fmap (\ o -> Var (outputType o) (outputName o)) (outputs g))
  $ case nonEmpty (letBindings g) of
      Nothing -> Left False
      Just eqns -> Right $ fmap (\ (os, rhs) ->
        (fmap (\ o -> PVar (outputType o) (outputName o)) os)
                :=:
                ((:| []) $ case rhs of
                   Alias ty x -> Var ty x
                   Copy ty x -> let n = length os in Cab (Cable (replicate n ty)) (replicate n (Var ty (inputName x)))
                   Call tys f es -> App tys f (map (\ i -> Var (inputType i) (inputName i)) es)
                   FanIn os   -> let ty = Cable (map inputType os) in
                                 Cab ty (map (\ i -> Var (inputType i) (inputName i)) os)
                   FanOut i   -> Var (inputType i) (inputName i)
                ))
                     eqns

toANF :: TypedDef -> TypedDef
toANF d = fromMaybe d $ uncurry fromGate <$> toGate d

------------------------------------------------------------------------------
-- Tests

test :: IO ()
test = do
  let runner = putStrLn . basicShow . toANF
  runner foo
  runner and4
  runner and4'
