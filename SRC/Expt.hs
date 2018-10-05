------------------------------------------------------------------------------
-----                                                                    -----
-----     Expt: Experiments on Syrup Programs                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Syrup.SRC.Expt where

import Data.List
import Data.Monoid
import Control.Monad.Identity
import Data.Function
import Control.Arrow

import Syrup.SRC.BigArray
import Syrup.SRC.Syn
import Syrup.SRC.Ty
import Syrup.SRC.Va


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

partitionSet :: (Ord x, Ord y) => (x -> y) -> Set x -> Arr y (Set x)
partitionSet f = foldMapSet $ \ x -> single (f x, singleton x)

groupArr :: (Ord k, Ord x) => (v -> x) -> Arr k v -> Arr x (Set k)
groupArr f = foldMapArr (\ (k, v) -> single (f v, singleton k))

type PState v = (Arr Integer (Set v), (Arr v Integer, Integer))

rekeyMap :: (Ord k, Ord v) =>
            (Arr v Integer, Integer) -> Arr k (Set v) -> PState v
rekeyMap vin ksv =
  appEndo
  ( foldMapArr
    (\ (k, sv) -> Endo
      (\ (isv, (vi, n)) ->
         ( insertArr (n, sv) isv
         , ( appEndo (foldMapSet (\ v -> Endo (insertArr (v, n))) sv) vi
           , n + 1
         ) )
    ) )
    ksv)
  (emptyArr, vin)

refinePState :: (Ord v, Ord w) => (v -> w) -> PState v -> PState v
refinePState f (isv, (_, n)) =
  appEndo
  ( foldMapArr
    (\ (i, sv) -> Endo
      (\ (isv, vin) ->
         let (isv', vin') = rekeyMap vin (partitionSet f sv)
         in  (isv' <> isv, vin') 
    ) )
    isv )
  (emptyArr, (emptyArr, n))

abstractStates :: Compo -> Arr Integer (Set [Va], [([Va], Integer)])
abstractStates c = go start
  where
    inTab = traverse tyVas (inpTys c)
    observeO :: [Va] -> [[Va]]
    observeO m = [snd (unstage c (m, i)) | i <- inTab]
    start = refinePState observeO
      ( single (0, foldMap singleton (traverse tyVas (memTys c)))
      , (emptyArr, 1)
      )
    go ps@(isv, (vi, _)) =
      if sizeArr isv == sizeArr isv' then stop ps else go ps' where
        ps'@(isv', _) = refinePState observeS ps
        observeS m = [findArr (fst (unstage c (m, i))) vi | i <- inTab]
    stop (isv, (vi, _)) = fmap glom isv where
      glom sm = (sm, [see i | i <- inTab]) where
        Just m = setElt sm
        see i = (o, s) where
          (n, o) = unstage c (m, i)
          Just s = findArr n vi

rankBySize :: Ord k => Arr k (Set v) -> [(k, [v])]
rankBySize =
  sortBy (compare `on` \ (k, vs) -> (length vs, k)) .
  foldMapArr (\ (k, sv) -> [(k, foldMapSet (:[]) sv)])

picks :: [x] -> [(x, [x])]
picks [] = []
picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

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
    go l2r ((lv, ls) : gl) r2l ((rv, rs) : gr) = case compare lv rv of
      LT -> []
      GT -> []
      EQ -> mo l2r ls gl r2l rs gr
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
