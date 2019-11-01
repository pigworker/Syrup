------------------------------------------------------------------------------
-----                                                                    -----
-----     Expt: Experiments on Syrup Programs                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ExistentialQuantification #-}

module Syrup.SRC.Expt where

import Data.List
import Data.Monoid
import Data.Traversable
import Control.Monad.Identity
import Control.Monad.State
import Data.Function
import Control.Arrow
import Data.Maybe
import qualified Data.Bifunctor as Bi

import Syrup.SRC.BigArray
import Syrup.SRC.Syn
import Syrup.SRC.Ty
import Syrup.SRC.Va
import Syrup.SRC.Utils

------------------------------------------------------------------------------
-- experiments
------------------------------------------------------------------------------

experiment :: CoEnv -> EXPT -> [String]
experiment g (Tabulate x) = case findArr x g of
  Nothing -> ["I don't know what " ++ x ++ " is."]
  Just c ->  ["Truth table for " ++ x ++ ":"] ++
             displayTab (tabCompo c)
experiment g (Simulate x m0 iss) = case findArr x g of
  Nothing -> ["I don't know what " ++ x ++ " is."]
  Just c ->  ["Simulation for " ++ x ++ ":"] ++
             runCompo c m0 iss
experiment g (Bisimilarity l r) = case (findArr l g, findArr r g) of
  (Nothing, _) -> ["I don't know what " ++ l ++ " is."]
  (_, Nothing) -> ["I don't know what " ++ r ++ " is."]
  (Just lc, Just rc) ->
    report (l, r) (bisimReport lc rc)


------------------------------------------------------------------------------
-- running tine sequences
------------------------------------------------------------------------------

runCompo :: Compo -> [Va] -> [[Va]] -> [String]
runCompo c m0 iss
  | not (tyVaChks mTys m0)
  = [ concat ["Memory for ", monick c, " has type {",
              csepShow mTys, "}"]
    , concat ["That can't store {", foldMap show m0, "}."]
    ]
  | Just is <- find (not . tyVaChks iTys) iss
  = [ concat ["Inputs for ", monick c, " are typed ",
              csepShow iTys, ""]
    , concat ["That can't accept ", foldMap show is, "."]
    ]
  | otherwise = render (go 0 m0 iss)
  where
    mTys = memTys c
    iTys = inpTys c
    oTys = oupTys c
    go t mt [] = ([], (t, mt))
    go t mt (is : iss) = ((t, mt, is, os) : xs , (z, mo)) where
      (xs, (z, mo)) = go (t + 1) mt' iss
      (mt', os) = unstage c (mt, is)
    render (xs, (z, mo)) = map row xs ++ [lastrow]
      where
        w = length (show z)
        showtime t = reverse . take w  $ reverse (show t) ++ repeat ' '
        row (t, m, is, os) = concat
          [ showtime t, " {", foldMap show m, "} "
          , foldMap show is, " -> ", foldMap show os
          ]
        lastrow = concat
          [ showtime z, " {", foldMap show mo, "}" ]



tyVaChks :: [Ty1] -> [Va] -> Bool
tyVaChks ts vs = length ts == length vs && all id (zipWith tyVaChk ts vs)

tyVaChk :: Ty1 -> Va -> Bool
tyVaChk (Bit ()) V0 = True
tyVaChk (Bit ()) V1 = True
tyVaChk (Cable ts) (VC vs) = tyVaChks ts vs
tyVaChk _ _ = False


------------------------------------------------------------------------------
-- tabulating behaviours of components
------------------------------------------------------------------------------

tabCompo :: Compo -> [( [Va]  -- inputs in
                      , [( [Va]  -- memory in
                         , ( [Va]  -- memory out
                           , [Va]  -- outputs out
                           ))])]
tabCompo c =
  [ (ii, [ (mi, unstage c (mi, ii))
         | mi <- meTab
         ] )
  | ii <- inTab
  ]
  where
    inTab = traverse tyVas (inpTys c)
    meTab = traverse tyVas (memTys c)

displayTab :: [( [Va]  -- inputs in
                      , [( [Va]  -- memory in
                         , ( [Va]  -- memory out
                           , [Va]  -- outputs out
                           ))])]
           -> [String]
displayTab = (>>= go) where
  go (is, [([],([], os))]) = [foldMap show is ++ " | " ++ foldMap show os]
  go (is, xs) = zipWith (++)
      (l : repeat (replicate (length l) ' '))
      [ concat [" { ", foldMap show mi, " -> ", foldMap show mo, " } ",
                foldMap show os]
      | (mi, (mo, os)) <- xs
      ]
    where l = foldMap show is


------------------------------------------------------------------------------
-- generating input values from types
------------------------------------------------------------------------------

tyVas :: Ty1 -> [Va]
tyVas (Bit _)    = [V0, V1]
tyVas (Cable ts) = VC <$> traverse tyVas ts


------------------------------------------------------------------------------
-- splicing output values from types
------------------------------------------------------------------------------

spliceVas :: [Ty2] -> [Va] -> [Va] -> [Va]
spliceVas [] _ _ = []
spliceVas (Bit T0 : ts) (v : vs) ws = v : spliceVas ts vs ws
spliceVas (Bit T1 : ts) vs (w : ws) = w : spliceVas ts vs ws
spliceVas (Cable ts' : ts) (VC vs' : vs) (VC ws' : ws) =
  VC (spliceVas ts' vs' ws') : spliceVas ts vs ws


------------------------------------------------------------------------------
-- unstaging a component
------------------------------------------------------------------------------

unstage :: Compo -> ([Va], [Va]) -> ([Va], [Va])
unstage c (mi, ii) = (mo, oo) where
  o0 = stage0 c mi
  moo1 = stage1 c (mi ++ ii)
  (mo, o1) = splitAt (length (memTys c)) moo1
  oo = spliceVas (oupTys c) o0 o1


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
    inTab = traverse tyVas (inpTys c)
    observeO :: [Va] -> [[Va]]
    observeO m = [snd (unstage c (m, i)) | i <- inTab]
    start = refinePState observeO $
      let startvs = foldMap singleton (traverse tyVas (memTys c))
          vsClass = 0 <$ startvs
      in PState (single (0, startvs)) vsClass 1
    go ps@(PState isv vi _) =
      if sizeArr isv == sizeArr isv' then stop ps else go ps' where
        ps'@(PState isv' _ _) = refinePState observeS ps
        observeS m = [findArr (fst (unstage c (m, i))) vi | i <- inTab]
    stop (PState isv vi _) = fmap glom isv where
      glom sm = (sm, [see i | i <- inTab]) where
        Just m = setElt sm
        see i = (o, s) where
          (n, o) = unstage c (m, i)
          Just s = findArr n vi

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
  deriving Show

data Report = forall st st'.
  (Ord st, Ord st', Show st, Show st') => Report (Report' st st')

report :: (String, String) -> Report -> [String]
report (lnom, rnom) (Report (Incompatible (lis, los) (ris, ros))) =
  [lnom ++ " and " ++ rnom ++ " are incompatible"
  ,concat [lnom, "(", showTyList lis, ") -> ", showTyList los
          ]
  ,concat [rnom, "(", showTyList ris, ") -> ", showTyList ros
          ]
  ]
report (lnom, rnom) (Report (InstantKarma ins ml (l : _) ru mr)) =
  [lnom ++ " has a behaviour that " ++ rnom ++ " does not match"]
  ++ mem
  ++ foldMapArr grot mr
  where
    (loss, mem) = case findArr l ml of
      Just (lvas, loss) -> (,) loss $ case leftmostArr lvas of
        Just [] -> []
        Just vs -> ["in memory state {" ++ foldMap show vs ++ "}"]

    grot :: forall st. Ord st => (st, (Set [Va], [([Va], st)])) -> [String]
    grot (r, (rvas, ross)) = (++ grump) $ case leftmostArr rvas of
      Just [] -> []
      _ -> ["when " ++ rnom ++ " has memory like {"
            ++ intercalate "," (foldMapSet statesh rvas) ++ "}"]
      where
        grump = foldMap screp
          [ (is, (los, ros))
          | (is, ((los, _), (ros, _))) <- zip ins (zip loss ross)
          , los /= ros
          ]
    screp (is, (los, ros)) = (:[]) $ concat
      ["  "
      ,lnom,"(",foldMap show is,") = ", foldMap show los
      , " but "
      ,rnom,"(",foldMap show is,") = ", foldMap show ros
      ]
report (lnom, rnom) (Report (InstantKarma ins ml [] (r : _) mr)) =
  [rnom ++ " has a behaviour that " ++ lnom ++ " does not match"]
  ++ mem
  ++ foldMapArr grot ml
  where
    (ross, mem) = case findArr r mr of
      Just (rvas, ross) -> (,) ross $ case leftmostArr rvas of
        Just [] -> []
        Just vs -> ["in memory state {" ++ foldMap show vs ++ "}"]
    grot (l, (lvas, loss)) = (++ grump) $ case leftmostArr lvas of
      Just [] -> []
      _ -> ["when " ++ lnom ++ " has memory like {"
            ++ intercalate "," (foldMapSet statesh lvas) ++ "}"]
      where
        grump = foldMap screp
          [ (is, (los, ros))
          | (is, ((los, _), (ros, _))) <- zip ins (zip loss ross)
          , los /= ros
          ]
    screp (is, (los, ros)) = (:[]) $ concat
      ["  "
      ,rnom,"(",foldMap show is,") = ", foldMap show ros
      , " but "
      ,lnom,"(",foldMap show is,") = ", foldMap show los
      ]
report (lnom, rnom) (Report (CounterModel ml (Left (l, rss)) mr)) =
  [lnom ++ " can be distinguished from all possible states of " ++ rnom
  ,"when " ++ lnom ++ " has memory {" ++ lmem ++ "}"
  ] ++ foldMapArr grump rss
  where
  lmem = case findArr l ml of
    Just (lvas, _) -> case leftmostArr lvas of
      Just lmem -> foldMap show lmem
  grump (r, vss) =
    ["if " ++ rnom ++ " has memory like {" ++
     intercalate "," (foldMapSet statesh rs) ++ "}, try inputs " ++
     intercalate ";" (fmap (foldMap show) vss)
    ] where Just (rs, _) = findArr r mr
report (lnom, rnom) (Report (CounterModel ml (Right (r, lss)) mr)) =
  [rnom ++ " can be distinguished from all possible states of " ++ lnom
  ,"when " ++ rnom ++ " has memory {" ++ rmem ++ "}"
  ] ++ foldMapArr grump lss
  where
  rmem = case findArr r mr of
    Just (rvas, _) -> case leftmostArr rvas of
      Just rmem -> foldMap show rmem
  grump (l, vss) =
    ["if " ++ lnom ++ " has memory like {" ++
     intercalate "," (foldMapSet statesh ls) ++ "}, try inputs " ++
     intercalate ";" (fmap (foldMap show) vss)
    ] where Just (ls, _) = findArr l ml
report (lnom, rnom) (Report (Bisimilar ml (Bisim l2r _) mr)) =
  [lnom ++ " behaves like " ++ rnom] ++ foldMapArr simState l2r
  where
    simState (l, r) = case (findArr l ml, findArr r mr) of
      (Just (ls, _), Just (rs, _)) -> (:[]) $ concat
        [  "  {"
        ,  intercalate "," (foldMapSet statesh ls)
        ,  "} ~ {"
        ,  intercalate "," (foldMapSet statesh rs)
        ,  "}"
        ]

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
    where

      -- phase 0 check types
      lit = inpTys cl              -- left  input  types
      lot = map fogTy (oupTys cl)  -- left  output types
      rit = inpTys cr              -- right input  types
      rot = map fogTy (oupTys cr)  -- right output types
      ins = traverse tyVas lit     -- tabulated input values

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
    where
      loss l = case findArr l ml of Just (_, loss) -> loss
      ross r = case findArr r mr of Just (_, ross) -> ross

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

{-
bisimulations :: Compo -> Compo
              -> [Arr Integer ((Set [Va], Set [Va]), [([Va], Integer)])]
bisimulations cl cr
  | inpTys cl /= inpTys cr || map stanTy (oupTys cl) /= map stanTy (oupTys cr)
  = []  -- not even type compatible
  | otherwise = map stop (go emptyArr gl emptyArr gr)
  where
    ml = abstractStates cl
    gl = rankBySize (groupArr (fmap fst . snd) ml)
    mr = abstractStates cr
    gr = rankBySize (groupArr (fmap fst . snd) mr)
    stop (l2r, r2l) = done where
      l2c = fst $ appEndo
          (foldMapArr
            (\ (l, _) -> Endo $ \ (l2c, c) -> (insertArr (l, c) l2c, c + 1))
            ml)
          (emptyArr, 0)
      flc l = c where Just c = findArr l l2c
      flr l = r where Just r = findArr l l2r
      frc r = flc l where Just l = findArr r r2l
      done = appEndo
      
        (foldMapArr
          (\ (l, (lms, ols)) ->
            let Just (rms, _) = findArr (flr l) mr
            in  Endo $ insertArr (flc l, ((lms, rms), fmap (id *** flc) ols)))
         ml)
        emptyArr
    go l2r [] r2l [] = [(l2r, r2l)]
    go l2r ((lv, ls) : gl) r2l ((rv, rs) : gr) | lv == rv =
      mo l2r ls gl r2l rs gr
    go _ _ _ _ = []
    mo l2r [] gl r2l [] gr = go l2r gl r2l gr
    mo _ [] _ _ _ _ = []
    mo _ _ _ _ [] _ = []
    mo l2r (l : ls) gl r2l rs gr = case findArr l l2r of
      Just r -> case partition (r ==) rs of
        ([_], rs) -> mo l2r ls gl r2l rs gr
        _ -> []
      Nothing -> do
        (r, rs) <- picks rs
        Nothing <- return (findArr r r2l)
        let (l2r', r2l') = (insertArr (l, r) l2r, insertArr (r, l) r2l)
        let Just (_, lt) = findArr l ml
        let Just (_, rt) = findArr r mr
        Just (l2r, r2l) <- return $
          foldr
            (\ (l, r) m -> m >>= \ (l2r, r2l) ->
               case (findArr l l2r, findArr r r2l) of
                 (Nothing, Nothing) ->
                    Just (insertArr (l, r) l2r, insertArr (r, l) r2l)
                 (Just _, Just l') | l == l' -> Just (l2r, r2l)
                 _ -> Nothing
            )
            (Just (l2r', r2l')) (zip (map snd lt) (map snd rt))
        mo l2r ls gl r2l rs gr

-}
