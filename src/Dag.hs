------------------------------------------------------------------------------
-----                                                                    -----
-----     Dag: directed acyclic graphs                                   -----
-----                                                                    -----
------------------------------------------------------------------------------

module Dag where

import Data.Maybe

import BigArray

newtype Dag n = Dag
  { upSets :: Arr n (Set n) -- map each node to its upward closure
  } deriving Show

upSet  :: Ord n => Dag n -> n -> Set n
upSet (Dag dag) n = fromMaybe mempty (findArr n dag)

downSet :: Ord n => Dag n -> n -> Set n
downSet (Dag dag) n =
  foldMapArr (\ (x, xup) -> if inSet n xup then singleton x else mempty) dag

invertDag :: Ord n => Dag n -> Dag n
invertDag (Dag dag) =
  Dag (foldMapArr (\ (x, _) -> single (x, downSet (Dag dag) x)) dag)

rawDelete :: Ord n => Set n -> Dag n -> Dag n
rawDelete ns (Dag dag) = Dag $
  foldMapArr (\ (x, xup) -> if inSet x ns then mempty else
      single (x,
        foldMapArr (\ (y, ()) -> if inSet y ns then mempty else singleton y)
          xup))
    dag

edge :: Ord n => (n {-x-}, n {-y-}) -> Dag n ->
  ( Set n -- the set of nodes thus identified with y and deleted
  , Dag n -- the updated dag
  )
edge (x, y) (Dag dag) = case findArr y dag of
  Nothing -> -- y does not exist, so
    let dag' = dag <> single (x, singleton x) <> single (y, singleton y)
          -- ensure that x and y both exist, then...
    in  (mempty,
         Dag $ dag' <>
          foldMapArr
          (\ (z, zup) -> if inSet x zup then single (z, singleton y) else mempty)
          dag'  -- ...add y to every upSet containing x
        )
  Just yup  -- y exists, with upSet yup
    | inSet x yup ->  -- x is above y, so some collapse is needed
      let yis = deleteArr y (intersectSet yup (downSet (Dag dag) x))
            -- everything above y and below x, apart from y
      in  (yis, rawDelete yis (Dag dag))
    | otherwise ->
      let dag' = dag <> single (x, singleton x) -- ensure that x exists
      in  ( mempty
          , Dag (dag' <>
                 foldMapArr (\ (z, zup) -> if inSet x zup then (single (z, yup)) else mempty)
                 dag')  -- everything with x in its upSet gets yup in its upSet, too
          )

upDelete :: Ord n => n -> Dag n -> (Set n, Dag n)
upDelete n (Dag dag) = case findArr n dag of
  Nothing -> (mempty, Dag dag)
  Just nup -> (nup, rawDelete nup (Dag dag))

downDelete :: Ord n => n -> Dag n -> (Set n, Dag n)
downDelete n dag =
  let ndn = downSet dag n
  in  (ndn, rawDelete ndn dag)
