------------------------------------------------------------------------------
-----                                                                    -----
-----     Expt: Experiments on Syrup Programs                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Syrup.Expt where

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (gets, StateT(StateT), execStateT, get, put, runStateT)
import Control.Monad.Writer (tell)

import qualified Data.Bifunctor as Bi
import Data.Foldable (toList)
import Data.Forget (forget)
import Data.Function (on)
import Data.List (find, intercalate, sortBy)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid (Endo(Endo), appEndo, Sum(Sum))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Data.Void (Void, absurd)

import Language.Syrup.Anf
import Language.Syrup.BigArray
import Language.Syrup.Cst
import Language.Syrup.DeMorgan (deMorgan)
import Language.Syrup.DNF (dnf, ttToDef)
import Language.Syrup.Doc hiding (AList, unwords)
import Language.Syrup.Dot
import Language.Syrup.Fdk
import Language.Syrup.Opt
import Language.Syrup.Pretty
import Language.Syrup.Syn
import Language.Syrup.Ty
import Language.Syrup.Unelab
import Language.Syrup.Utils

import Utilities.Lens
import Utilities.Nat
import Utilities.Vector

import System.Directory (findExecutable)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

------------------------------------------------------------------------------
-- experiments
------------------------------------------------------------------------------

impossible :: String -> a
impossible str = error ("The IMPOSSIBLE has happened: " ++ str)

type MonadExperiment s m =
  ( Has DotSt s
  , MonadReader Options m
  , MonadCompo s m
  )

withCompo :: MonadCompo s m
          => Name -> (Compo -> m ()) -> m ()
withCompo x k = gets (findArr x . (^. hasLens)) >>= \case
  Nothing -> tell $ Seq.singleton (AnUnknownIdentifier x)
  Just c -> k c

withImplem :: MonadCompo s m
           => Name -> (TypedDef -> m ()) -> m ()
withImplem x k = withCompo x $ \ c -> case defn c of
  Nothing -> tell $ Seq.singleton (AMissingImplementation x)
  Just i -> k i

ifSmallEnough :: MonadExperiment s m => Compo -> m () -> m ()
ifSmallEnough c act = do
  let cSize = sum (sizeBits . getInputType <$> inpTys c)
            + sum (sizeBits . getCellType  <$> memTys c)
  -- sneakily using the size we want if none was set by the user
  -- so as not to have to case on the (Maybe Int) :)
  lim <- asks (fromMaybe cSize . experimentLimit)
  if cSize <= lim then act else
    tell $ Seq.singleton (AnUnreasonablyLargeExperiment lim cSize (monick c))

experiment :: MonadExperiment s m => EXPT -> m ()
experiment (Tabulate x) = withCompo x $ \ c -> ifSmallEnough c $
  tell $ Seq.singleton $ ATruthTable x $ displayTabulation (tabulate c)
experiment (Simulate x m0 iss) = withCompo x $ \ c -> case runCompo c m0 iss of
  Left fdk -> tell $ Seq.singleton fdk
  Right msg -> anExperiment "Simulation for" [x] msg
experiment (UnitTest x is os) = withCompo x $ \ c ->
  tell $ Seq.singleton $ WhenUnitTesting x is os $ case unitTest c is os of
  Left fdk -> [fdk]
  Right () -> [ASuccessfulUnitTest]
experiment (Bisimilarity l r) = withCompo l $ \ lc -> withCompo r $ \ rc -> do
  anExperiment "Bisimulation between" [l, r] $
    report (l, r) (bisimReport lc rc)
experiment (Print x) = withImplem x $ \ i -> do
  g <- use hasLens
  tell $ Seq.singleton
    $ ARawCode "Printing" x
    $ prettyUnelabed g i
experiment (Typing x) = withCompo x $ \ c -> do
  g <- use hasLens
  anExperiment "Typing for" [x] $
    prettyBlock
      $ runUnelab g
      $ TypeDecl x
          (getInputType <$> inpTys c)
          (getOutputType <$> oupTys c)
experiment (Display xs x) = withImplem x $ \ i -> do
  st <- use hasLens
  let (fdk, circuit) = whiteBoxDef st (map getName xs) i
  unless (null fdk) $ tell $ Seq.singleton (WhenDisplaying x $ toList fdk)
  case circuit of
    Nothing -> pure ()
    Just dot -> asks graphFormat >>= \ opts ->
      tell $ Seq.singleton $ case opts of
        SourceDot -> ADotGraph xs x dot
        RenderedSVG -> unsafePerformIO $
          findExecutable "dot" >>= \case
            Nothing -> pure (ANoExecutable "dot")
            Just{} -> AnSVGGraph xs x . lines <$> readProcess "dot" ["-q", "-Tsvg"] (unlines dot)
experiment (Dnf x) = withImplem x $ \ i -> do
  env <- use hasLens
  tell $ Seq.singleton
    $ ARawCode "Disjunctive Normal Form of" x
    $ prettyUnelabed env $ dnf env i
experiment (Anf x) = withImplem x $ \ i -> do
  env <- use hasLens
  tell $ Seq.singleton
    $ ARawCode "A Normal Form of" x
    $ prettyUnelabed env $ toANF i
experiment (Costing nms x) = do
  g <- use hasLens
  let support = foldMap singleton nms
  let cost = costing g support x
  anExperiment "Cost for" [x] $
    flip foldMapArr cost (\ (x, Sum k) ->
      let copies = "cop" ++ if k > 1 then "ies" else "y" in
      aLine $$ [pretty k, " ", pretty copies, " of ", identifier x])
experiment (Simplify x) = withImplem x $ \ i -> do
  g <- use hasLens
  tell $ Seq.singleton
    $ ARawCode "Simplification of" x
    $ prettyUnelabed g (deMorgan g i)
experiment (FromOutputs f xs bs) = do
  g <- use hasLens
  case ttToDef g f (map getInputName xs) bs of
    Nothing -> tell $ Seq.singleton (AnInvalidTruthTableOutput f)
    Just def -> do
      tell $ Seq.singleton
        $ ARawCode "DNF circuit for" f
        $ prettyUnelabed g def

------------------------------------------------------------------------------
-- running tine sequences
------------------------------------------------------------------------------

data Simulation (n :: Nat) mem step = Simulation
  { simMemory :: mem
  , simSteps  :: Vector n step
  }

data TimeStep = TimeStep
  { currentTime    :: Int
  , currentMemory  :: [Va]
  , currentInputs  :: [Va]
  , currentOutputs :: [Va]
  }

instance Pretty (Simulation n (Int, [Va]) TimeStep) where
  type PrettyDoc (Simulation n (Int, [Va]) TimeStep) = Doc
  prettyPrec _ (Simulation (z, mo) steps) = foldMap row steps <> lastrow
    where
      w = length (show z)
      prettyTime t = pretty $ reverse . take w  $ reverse (show t) ++ repeat ' '
      row (TimeStep t mt is os) = aLine $$
        [ prettyTime t, " ", braces (foldMap pretty mt)
        , " "
        , foldMap pretty is, " -> ", foldMap pretty os
        ]
      lastrow = aLine $$ [ prettyTime z, " ", braces (foldMap pretty mo) ]

simulate :: Compo -> Simulation n [Va] [Va]
         -> Simulation n (Int, [Va]) TimeStep
simulate c (Simulation m0 iss) = go 0 m0 iss
  where

    go :: forall m. Int -> [Va] -> Vector m [Va] -> Simulation m (Int, [Va]) TimeStep
    go t mt VNil = Simulation (t, mt) VNil
    go t mt (is :* iss) =
      let (mt', os) = unstage c (mt, is) in
      let step = TimeStep
            { currentTime    = t
            , currentMemory  = mt
            , currentInputs  = is
            , currentOutputs = os
            } in
      let Simulation mEnd steps = go (t + 1) mt' iss in
      Simulation mEnd (step :* steps)

checkMemory :: Compo -> [Va] -> Either Feedback ()
checkMemory c m0
  | not (tyVaChks mTys m0)
  = Left (AnIllTypedMemory (monick c) mTys m0)
  | otherwise = pure ()

  where

    mTys = getCellType <$> memTys c

checkInputs :: Compo -> [[Va]] -> Either Feedback ()
checkInputs c iss
  | Just is <- find (not . tyVaChks iTys) iss
  = Left (AnIllTypedInputs (monick c) iTys is)
  | otherwise = pure ()
  where
    iTys = getInputType <$> inpTys c

checkOutputs :: Compo -> [Va] -> Either Feedback ()
checkOutputs c os
  | not (tyVaChks oTys os)
  = Left (AnIllTypedOutputs (monick c) oTys os)
  | otherwise = pure ()
  where
    oTys = getOutputType <$> oupTys c

unitTest :: Compo -> CircuitConfig -> CircuitConfig -> Either Feedback ()
unitTest c (CircuitConfig mi is) (CircuitConfig mo os) = do
  checkMemory c mi
  checkInputs c [is]
  checkMemory c mo
  checkOutputs c os
  case simulate c (Simulation mi (is :* VNil)) of
    Simulation (_, mo') (step :* VNil)
      | mo /= mo' -> Left (AWrongFinalMemory mo mo')
      | os /= currentOutputs step -> Left (AWrongOutputSignals os (currentOutputs step))
      | otherwise -> pure ()

runCompo :: Compo -> [Va] -> [[Va]] -> Either Feedback Doc
runCompo c m0 iss = case fromList iss of
  AList xs -> do
    checkMemory c m0
    checkInputs c iss
    pure $ pretty $ simulate c (Simulation m0 xs)


tyVaChks :: [Ty t Void] -> [Va] -> Bool
tyVaChks ts vs = length ts == length vs && and (zipWith tyVaChk ts vs)

tyVaChk :: Ty t Void -> Va -> Bool
tyVaChk (Bit _) V0 = True
tyVaChk (Bit _) V1 = True
tyVaChk (Cable ts) (VC vs) = tyVaChks ts vs
tyVaChk (Meta x) _ = absurd x
-- Type unfolding
tyVaChk (TVar _ t) v = tyVaChk t v
-- Type errors
tyVaChk (Bit _) (VC _) = False
tyVaChk (Cable _) V0 = False
tyVaChk (Cable _) V1 = False
tyVaChk _ VQ = False -- impossible?

------------------------------------------------------------------------------
-- tabulating behaviours of components
------------------------------------------------------------------------------

data TabRow = TabRow
  { currentCells :: [Va] -- current value for the memory cells
  , nextCells    :: [Va] -- next value for the memory cells
  , outputValues :: [Va] -- output values
  }

data Tabulation = Tabulation
  { -- headers
    tabbedInputs  :: [InputWire]
  , tabbedCells   :: [MemoryCell]
  , tabbedOutputs :: [OutputWire]
    -- actual content
  , tabbedRows    :: [([Va]      -- inputs
                      , [TabRow] -- rows
                      )]
  }

type Template = Ty Void Int

data RowTemplate = RowTemplate
  { inputTemplates  :: [Template]
  , cellTemplates   :: [Template]
  , outputTemplates :: [Template]
  }

-- Generate a template from a pattern and its type
template :: Pat' ty String -> Ty a Void -> Template
template _           (Meta x)   = absurd x
template (PVar _ v)  t          = Meta (max (length v) (sizeTy t)) -- ?!
template p           (TVar s t) = template p t
template (PCab _ ps) (Cable ts) = Cable (zipWith template ps ts)
template (PCab _ _) (Bit _) = impossible "ill typed pattern"

mTemplate :: Maybe (Pat' ty String) -> Ty a Void -> Template
mTemplate Nothing  t = Meta (sizeTy t)
mTemplate (Just p) t = template p t

inputTemplate :: InputWire -> Template
inputTemplate (InputWire p t) = mTemplate p t

getCellPat :: MemoryCell -> Maybe Pat
getCellPat = fmap (PVar () . cellName) . getCellName

cellTemplate :: MemoryCell -> Template
cellTemplate c@(MemoryCell _ t) = mTemplate (getCellPat c) t

outputTemplate :: OutputWire -> Template
outputTemplate (OutputWire p t) = mTemplate (fmap (fst <$>) p) t

-- `displayPat ts ps` PRECONDITION: ts was generated using ps
displayPat :: Template -> Pat' ty String -> String
displayPat (TVar _ t) p           = displayPat (forget t) p
displayPat (Meta s)   (PVar _ n)  = padRight (s - length n) n
displayPat (Cable ts) (PCab _ ps) = "[" ++ unwords (zipWith displayPat ts ps) ++ "]"
displayPat (Bit x) _ = absurd x
displayPat (Meta _) (PCab _ _) = impossible "displaying a cable pattern at a meta type"
displayPat (Cable _) (PVar _ _) = impossible "displaying a variable pattern at a cable type"

displayMPat :: Template -> Maybe (Pat' ty String) -> String
displayMPat t = maybe (displayEmpty t) (displayPat t)

displayEmpty :: Template -> String
displayEmpty t = replicate (sum t) ' '

displayVa :: Template -> Va -> String
displayVa (Meta s)   v       = let n = show v in padRight (s - length n) n
displayVa (TVar _ t) v       = displayVa (forget t) v
displayVa (Cable ts) (VC vs) = "[" ++ displayVas ts vs ++ "]"
displayVa (Cable _) _ = impossible "ill typed cable value"
displayVa (Bit x) _ = absurd x

displayVas :: [Template] -> [Va] -> String
displayVas ts vs = unwords $ zipWith displayVa ts vs

displayRow :: RowTemplate -> ([Va], [TabRow]) -> [String]
displayRow tmp (vs, [TabRow [] [] os]) =
  [ displayVas (inputTemplates tmp) vs
  ++ " | "
  ++ displayVas (outputTemplates tmp) os
  ]
displayRow tmp (vs, trs) = zipWith (++) (inputs : padding) transitions where

  padding     = repeat (replicate (length inputs) ' ')
  inputs      = displayVas (inputTemplates tmp) vs
  transitions =
    [ concat [ " { " , displayVas (cellTemplates tmp)   ccs
             , " -> ", displayVas (cellTemplates tmp)   ncs
             , " } " , displayVas (outputTemplates tmp) os
             ]
    | TabRow ccs ncs os <- trs
    ]

displayTabulation :: Tabulation -> [String]
displayTabulation (Tabulation ins mes ous rs) =
  header ++ rows where

  header = [ inputs
              ++ states
              ++ outputs
            ]
         ++ [ replicate (length inputs) '-'
              ++ statesSep
              ++ replicate (length outputs) '-'
            ]
  rows   = concatMap (displayRow template) rs

  template = RowTemplate
    { inputTemplates  = tINS
    , cellTemplates   = tMEM
    , outputTemplates = tOUT
    }

  states    = if null mes then " | " else " { " ++ cells ++ " -> " ++ cells ++ " } "
  statesSep = if null mes then "-|-" else "-{-" ++ replicate (4 + 2 * length cells) '-' ++ "-}-"

  -- templates
  tINS = map inputTemplate ins
  tMEM = map cellTemplate mes
  tOUT = map outputTemplate ous

  -- actual tabulated values
  inputs    = unwords $ zipWith (\ t -> displayMPat t . getInputPat)  tINS ins
  cells     = unwords $ zipWith (\ t -> displayMPat t . getCellPat)   tMEM mes
  outputs   = unwords $ zipWith (\ t -> displayMPat t . fmap (fst <$>) . getOutputPat) tOUT ous

tabulate :: Compo -> Tabulation
tabulate c = Tabulation (inpTys c) (memTys c) (oupTys c)
  [ (ii, [ uncurry (TabRow mi) (unstage c (mi, ii))
         | mi <- meTab
         ]
    )
  | ii <- inTab
  ] where
    inTab = traverse tyVas (getInputType <$> inpTys c)
    meTab = traverse tyVas (getCellType  <$> memTys c)

------------------------------------------------------------------------------
-- generating input values from types
------------------------------------------------------------------------------

tyVas :: Ty1 -> [Va]
tyVas (Meta x)   = absurd x
tyVas (TVar s t) = tyVas t
tyVas (Bit _)    = [V0, V1]
tyVas (Cable ts) = VC <$> traverse tyVas ts


------------------------------------------------------------------------------
-- splicing output values from types
------------------------------------------------------------------------------

spliceVas :: [Ty2] -> [Va] -> [Va] -> [Va]
spliceVas [] _ _ = []
spliceVas (Meta x  : _) _ _ = absurd x
spliceVas (TVar s t : ts) vs ws = spliceVas (t : ts) vs ws
spliceVas (Bit T0 : ts) (v : vs) ws = v : spliceVas ts vs ws
spliceVas (Bit T1 : ts) vs (w : ws) = w : spliceVas ts vs ws
spliceVas (Cable ts' : ts) (VC vs' : vs) (VC ws' : ws) =
  VC (spliceVas ts' vs' ws') : spliceVas ts vs ws
spliceVas (Cable _ : _) (_ : _) (_ : _) =
  impossible "ill typed cable values"
spliceVas (_ : _) [] _ = impossible "arity mismatch"
spliceVas (_ : _) _ [] = impossible "arity mismatch"


------------------------------------------------------------------------------
-- unstaging a component
------------------------------------------------------------------------------

unstage :: Compo -> ([Va], [Va]) -> ([Va], [Va])
unstage c (mi, ii) = (mo, oo) where
  o0 = stage0 c mi
  moo1 = stage1 c (mi ++ ii)
  (mo, o1) = splitAt (length (memTys c)) moo1
  oo = spliceVas (getOutputType <$> oupTys c) o0 o1


------------------------------------------------------------------------------
-- computing the abstract states of a component
------------------------------------------------------------------------------

-- Invariants:
-- The 'next' state should be in the support of the array

type AbstractCompo' st =
  (Arr st             -- an abstract state
      ( Set [Va]      -- its corresponding memory states (nonempty)
      , [([Va], st)]  -- its output and next state, per input
      ))

data AbstractCompo = forall st. (Ord st, Show st) => AC (AbstractCompo' st)
deriving instance Show AbstractCompo

partitionSet :: (Ord x, Ord y) => (x -> y) -> Set x -> Arr y (Set x)
partitionSet f = foldMapSet $ \ x -> single (f x, singleton x)

groupArr :: (Ord k, Ord x) => (v -> x) -> Arr k v -> Arr x (Set k)
groupArr f = foldMapArr (\ (k, v) -> single (f v, singleton k))

-- Invariants:
-- fresh not in the support of preClasses
-- v \in preClasses !! i <=> preClassLookup !! v = Just i

data PState v = PState
  { preClasses     :: Arr Integer (Set v)
  , preClassLookup :: Arr v Integer
  , fresh          :: Integer
  }

emptyPState :: PState v
emptyPState = PState emptyArr emptyArr 0

rekeyMap :: (Ord k, Ord v) =>
            Arr k (Set v) -> PState v -> PState v
rekeyMap ksv ps =
  appEndo
  ( foldMapArr
    (\ (_, sv) -> Endo
      (\ (PState isv vi n) ->
         PState (insertArr (n, sv) isv)
                (appEndo (foldMapSet (\ v -> Endo (insertArr (v, n))) sv) vi)
                (n + 1)
    ) )
    ksv)
  ps

refinePState :: (Ord v, Ord w) => (v -> w) -> PState v -> PState v
refinePState f (PState isv _ _) =
  appEndo
  ( foldMapArr
      (\ (_, sv) -> Endo (rekeyMap (partitionSet f sv)))
      isv
  )
  emptyPState

abstractStates :: Compo -> AbstractCompo
abstractStates c = AC $ go start
  where
    inTab = traverse tyVas (getInputType <$> inpTys c)
    observeO :: [Va] -> [[Va]]
    observeO m = [snd (unstage c (m, i)) | i <- inTab]
    start = refinePState observeO $
      let startvs = foldMap singleton (traverse tyVas (getCellType <$> memTys c))
          vsClass = 0 <$ startvs
      in PState (single (0, startvs)) vsClass 1
    go ps@(PState isv vi _) =
      if sizeArr isv == sizeArr isv' then stop ps else go ps' where
        ps'@(PState isv' _ _) = refinePState observeS ps
        observeS m = [findArr (fst (unstage c (m, i))) vi | i <- inTab]
    stop (PState isv vi _) = fmap glom isv where
      glom sm = (sm, [x | i <- inTab, x <- see i]) where
        m = fromJust (setElt sm)
        see i =
          let (n, o) = unstage c (m, i)
          in  case findArr n vi of
                 Just s -> [(o, s)]
                 _ -> []

whyDiffer :: forall st. Ord st => AbstractCompo' st
          -> [[Va]]             -- tabulated inputs
          -> (st, st)           -- should both be distinct but defined
          -> [[Va]]             -- shortest discriminating sequence
whyDiffer ac ti xy = head (sortBy (compare `on` length) (go emptyArr xy))
  where
    go :: Set (st, st) -> (st, st) -> [[[Va]]]
    go seen xy@(x, y) = case findArr xy seen of
      Just _ -> []
      Nothing -> case (findArr x ac, findArr y ac) of
        (Just (_, xoss), Just (_, yoss)) ->
          let blah = zip ti (zip xoss yoss)
          in  case [is | (is, ((xos, xn), (yos, yn))) <- blah, xos /= yos] of
                is : _ -> [[is]]
                [] -> [ is : iss
                      | (is, ((xos, xn), (yos, yn))) <- blah, xn /= yn
                      , iss <- go (singleton xy <> seen) (xn, yn)
                      ]
        _ -> impossible "could not generate whyDiffer explanation"


------------------------------------------------------------------------------
-- computing bisimulations between two components
------------------------------------------------------------------------------

data Bisim x y = Bisim (Arr x y) (Arr y x) deriving Show
  -- INVARIANT is that if x maps to y, y maps to x

inBisim :: (Ord x, Ord y) => (x, y) -> Bisim x y -> Bool
inBisim (x, y) (Bisim x2y _) = case findArr x x2y of
  Just y' -> y' == y
  _ -> False

extBisim :: (Ord x, Ord y) => (x, y) -> Bisim x y -> Maybe (Bisim x y)
extBisim (x, y) b@(Bisim x2y y2x) = case (findArr x x2y, findArr y y2x) of
  (Nothing, Nothing) ->
    Just (Bisim (insertArr (x, y) x2y) (insertArr (y, x) y2x))
  (Just y', _) | y' == y -> Just b
  _ -> Nothing

data Report' st st'
  = Incompatible ([Ty1], [Ty1]) ([Ty1], [Ty1])
  | InstantKarma [[Va]] -- input table
      (AbstractCompo' st) -- one of the following is nonempty
      [st]   -- each of these lefts disputes output with all rights
      [st']  -- each of these rights disputes output with all lefts
      (AbstractCompo' st')
  | CounterModel
      (AbstractCompo' st)
      (Either (st , Arr st' [[Va]])
              (st', Arr st  [[Va]]))
      (AbstractCompo' st')
  | Bisimilar (AbstractCompo' st) (Bisim st st') (AbstractCompo' st')


data Report = forall st st'.
  (Ord st, Ord st', Show st, Show st') => Report (Report' st st')

report :: (Name, Name) -> Report -> Doc
report (lnom, rnom) (Report (Incompatible (lis, los) (ris, ros))) =
  aLine $$
    [ identifier lnom, " and ", identifier rnom, " are incompatible:"]
  <> aLine (isCode $ pretty (TypeDecl (StandardName lnom) lis los))
  <> aLine (isCode $ pretty (TypeDecl (StandardName rnom) ris ros))
report (lnom, rnom) (Report (InstantKarma ins ml (l : _) ru mr)) =
  aLine $$
    [ identifier lnom, " has a behaviour that ", identifier rnom, " does not match"]
  <> mem
  <> foldMapArr grot mr
  where
    (loss, mem) = case fromJust $ findArr l ml of
      (lvas, loss) -> (loss,) $ case fromJust $ leftmostArr lvas of
        [] -> mempty
        vs -> aLine $$ ["in memory state", braces (foldMap pretty vs)]

    grot :: forall st. Ord st => (st, (Set [Va], [([Va], st)])) -> Doc
    grot (r, (rvas, ross)) = (<> grump) $ case leftmostArr rvas of
      Just [] -> mempty
      _ -> aLine $$
        [ "when ", identifier rnom, " has memory like"
        , pretty (ASet $ foldMapSet statesh rvas)
        ]
      where
        grump = foldMap screp
          [ (is, (los, ros))
          | (is, ((los, _), (ros, _))) <- zip ins (zip loss ross)
          , los /= ros
          ]
    screp :: ([Va], ([Va], [Va])) -> Doc
    screp (is, (los, ros)) =
      let inputs = CircuitConfig [] is in
      aLine $$
        [ isCode $ circuitExec lnom inputs (CircuitConfig [] los)
        , " but "
        , isCode $ circuitExec rnom inputs (CircuitConfig [] ros)
        ]
report (lnom, rnom) (Report (InstantKarma ins ml [] (r : _) mr)) =
  aLine $$ [ identifier rnom, " has a behaviour that ", identifier lnom, " does not match" ]
  <> mem
  <> foldMapArr grot ml
  where
    (ross, mem) = case fromJust $ findArr r mr of
      (rvas, ross) -> (,) ross $ case fromJust $ leftmostArr rvas of
        [] -> mempty
        vs -> aLine $$ ["in memory state ", braces (foldMap pretty vs)]
    grot (l, (lvas, loss)) = (<> grump) $ case leftmostArr lvas of
      Just [] -> mempty
      _ -> aLine $$
             [ "when ", identifier lnom, " has memory like "
             , pretty (ASet $ foldMapSet statesh lvas)
             ]
      where
        grump = foldMap screp
          [ (is, (los, ros))
          | (is, ((los, _), (ros, _))) <- zip ins (zip loss ross)
          , los /= ros
          ]

    screp :: ([Va], ([Va], [Va])) -> Doc
    screp (is, (los, ros)) =
      aLine $$
        [ isCode $ circuitExec lnom (CircuitConfig [] is) (CircuitConfig [] los)
        , " but "
        , isCode $ circuitExec rnom (CircuitConfig [] is) (CircuitConfig [] ros)
        ]
report _ (Report (InstantKarma _ _ [] [] _)) =
  impossible "instant karma with no evidence"
report (lnom, rnom) (Report (CounterModel ml (Left (l, rss)) mr)) =
  aLine $$
    [ identifier lnom, " can be distinguished from all possible states of ", identifier rnom ]
  <> aLine $$
    [ "when ", identifier lnom, " has memory ", isCode (braces lmem) ]
  <> foldMapArr grump rss
  where
  lmem = case fromJust $ findArr l ml of
    (lvas, _) -> case fromJust $ leftmostArr lvas of
      lmem -> foldMap pretty lmem
  grump (r, vss) = aLine $$
    [ "if ", identifier rnom
    , " has memory like ", isCode $ braces $$ map pretty (foldMapSet statesh rs)
    , ", try inputs ", isCode $ punctuate ";" (fmap (foldMap pretty) vss)
    ] where rs = fst $ fromJust $ findArr r mr
report (lnom, rnom) (Report (CounterModel ml (Right (r, lss)) mr)) =
  aLine $$
    [ identifier rnom, " can be distinguished from all possible states of ", identifier lnom
    , "when ", identifier rnom, " has memory ", isCode (braces rmem)
    ]
  <> foldMapArr grump lss
  where
  rmem = case fromJust $ findArr r mr of
    (rvas, _) -> case fromJust $ leftmostArr rvas of
      rmem -> foldMap pretty rmem
  grump (l, vss) = aLine $$
    [ "if ", identifier lnom
    , " has memory like ", isCode $ braces $$ map pretty (foldMapSet statesh ls)
    , ", try inputs ", isCode $ punctuate ";" (fmap (foldMap pretty) vss)
    ] where ls = fst $ fromJust $ findArr l ml
report (lnom, rnom) (Report (Bisimilar ml (Bisim l2r _) mr)) =
  aLine $$ [ identifier lnom, " behaves like ", identifier rnom ]
  <> foldMapArr simState l2r
  where
    simState (l, r) = case (findArr l ml, findArr r mr) of
      (Just (ls, _), Just (rs, _)) -> nest 2 $ aLine $$
        [ isCode $ braces $$ fmap pretty (foldMapSet statesh ls)
        , " ~ "
        , isCode $ braces $$ fmap pretty (foldMapSet statesh rs)
        ]
      (_, _) -> aLine "The IMPOSSIBLE has happened: couldn't find states"

statesh :: [Va] -> [String]
statesh vs = [foldMap show vs]

bisimReport :: Compo -> Compo -> Report
bisimReport cl cr = case (abstractStates cl, abstractStates cr) of
 (AC ml, AC mr) -> Report (analysis ml mr) where

  analysis :: forall st st'. (Ord st, Ord st')
           => AbstractCompo' st -> AbstractCompo' st' -> Report' st st'
  analysis ml mr
    | (lit, lot) /= (rit, rot) = Incompatible (lit, lot) (rit, rot)
    | not (null lino && null rino) = InstantKarma ins ml lino rino mr
    | otherwise = case (lcOrBs, rcOrBs) of
      (Left (l, rvss), _) ->
        CounterModel
          ml
          (Left (l, imapArr (complete rvss) (fromJust (findArr l lido))))
          mr
      (_, Left (r, lvss)) ->
        CounterModel ml
          (Right (r, imapArr (complete lvss) (fromJust (findArr r rido))))
          mr
      (Right (b : _), _) -> Bisimilar ml b mr
      _ -> impossible "broken bisim analysis?!"
    where

      -- phase 0 check types
      lit = getInputType <$> inpTys cl               -- left  input  types
      lot = map (fogTy . getOutputType) (oupTys cl)  -- left  output types
      rit = getInputType <$> inpTys cr               -- right input  types
      rot = map (fogTy . getOutputType) (oupTys cr)  -- right output types
      ins = traverse tyVas lit                       -- tabulated input values

      -- phase 1 compute abstract machines
      -- cf. arguments ml and mr to analysis

      -- phase 2 check every state has a counterpart with same output
      lido = fmap                  -- lefts -:> rights -:> odiscs
        (\ (_, ib) -> fmap (outputDiscrepancies ib . snd) mr)
        ml
      lica :: Arr st [st']
      lica = fmap candidates lido  -- lefts -:> rights-with-no-odiscs
      lino = candidates lica       -- lefts-with-no-rights

      rido = fmap                  -- rights -:> lefts -:> odiscs
        (\ (_, ib) -> fmap (outputDiscrepancies ib . snd) ml)
        mr
      rica :: Arr st' [st]
      rica = fmap candidates rido  -- rights -:> lefts-with-no-odiscs
      rino = candidates rica       -- rights-with-no-lefts

      -- phase 3 grow possible bisims from left/right candidates
      lcOrBs = appEndo (foldMapArr (Endo . improve ml mr ins) lica)
                 (Right [Bisim emptyArr emptyArr])
      rcOrBs = appEndo (foldMapArr (Endo . improve mr ml ins) rica)
                 (Right [Bisim emptyArr emptyArr])

      -- phase 2 auxiliaries
      outputDiscrepancies :: forall a b.
           [([Va], a)]
        -> [([Va], b)]
        -> [([Va], ([Va], [Va]))]
      outputDiscrepancies ib ob =
        [ (is, (los, ros))
        | (is, ((los, _), (ros, _))) <- zip ins (zip ib ob)
        , los /= ros
        ]
      candidates :: forall a x. Arr a [x] -> [a]
      candidates = foldMapArr (\ (j, ds) -> if null ds then [j] else [])

-- phase 3 auxiliaries
improve :: forall a b. (Ord a, Ord b)
        => AbstractCompo' a -> AbstractCompo' b -> [[Va]]
        -> (a, [b])
        -> Either (a, [(b, [[Va]])])
                  [Bisim a b]
        -> Either (a, [(b, [[Va]])])
                  [Bisim a b]
improve ml mr ins = go where
  go _       (Left c)   = Left c  -- dead? aay dead
  go (l, rs) (Right bs) = case bs' of
    _ : _ -> Right bs'  -- we got some candidate bisims
    -- rs is nonempty, so if no Rights, some Lefts
    [] -> case [c | Left c <- ws] of
      c : _ -> Left (l, c)
      _ -> error "The IMPOSSIBLE has happened: neither Lefts nor Rights"
    where
      loss l = snd $ fromJust $ findArr l ml
      ross r = snd $ fromJust $ findArr r mr

      ws = map (tryCands l rs) bs
      bs' = [b | Right bs <- ws, b <- bs]
      tryCands :: a -> [b] -> Bisim a b
               -> Either [(b, [[Va]])]
                         [Bisim a b]
      -- try all the right-simulant candidates for a given left
      -- so we either find none will do or some plausible bisims
      tryCands l rs b = Bi.first (zip rs) $ allLeftsOrRight $
        map (\ r -> execStateT (growBis (l, r)) b) rs

      growBis :: (a, b)
              -> StateT (Bisim a b) (Either [[Va]]) ()
        -- grow a bisimulation by tracing from a candidate pair
      growBis (l, r) = do
        let tab = zip ins (zip (loss l) (ross r))
        case [vs | (vs, ((los, _), (ros, _))) <- tab, los /= ros] of
          vs : _ -> balk [vs]
          [] -> do
            Bisim l2r r2l <- get
            case (findArr l l2r, findArr r r2l) of
                -- r already something else
              (Just r', _) | r /= r' -> balk (whyDiffer mr ins (r, r'))
                -- l already something else
              (_, Just l') | l /= l' -> balk (whyDiffer ml ins (l, l'))
                -- (l, r) already in simulation
              (Just _, Just _) -> return ()
                -- (l, r) free to be added to simulation
              (Nothing, Nothing) -> do
                put (Bisim (insertArr (l, r) l2r) (insertArr (r, l) r2l))
                for tab $ \ (vs, ((_, l), (_, r))) ->
                  search vs $ growBis (l, r)
                return ()
              _ -> error "The IMPOSSIBLE has happened when trying to grow bisimulations"

      -- countermodel completion
complete :: Eq a
         => [(a, [[Va]])]                -- bad right traces
         -> (a, [([Va], ([Va], [Va]))])  -- right discrepancies
         -> [[Va]]                       -- right badness
complete _   (_, (vs, _) : _) = [vs]
complete rvss (r, _) = case lookup r rvss of
        Just vss  -> vss
        Nothing   -> []   -- this should not happen


balk :: e -> StateT s (Either e) x
balk e = StateT $ \ _ -> Left e

search :: t -> StateT s (Either [t]) x -> StateT s (Either [t]) x
search t p = StateT $ \ s -> case runStateT p s of
  Left ts -> Left (t : ts)
  Right o -> Right o


rankBySize :: Ord k => Arr k (Set v) -> [(k, [v])]
rankBySize =
  sortBy (compare `on` \ (k, vs) -> (length vs, k)) .
  foldMapArr (\ (k, sv) -> [(k, foldMapSet (:[]) sv)])

picks :: [x] -> [(x, [x])]
picks [] = []
picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]
