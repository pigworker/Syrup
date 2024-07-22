------------------------------------------------------------------------------
-----                                                                    -----
-----     Ded: Dead code elimination for Syrup                           -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Ded where

import Control.Monad (guard)

import Data.Maybe (fromMaybe)

import Language.Syrup.BigArray
import Language.Syrup.Syn
import Language.Syrup.Utils

cleanup :: Def' ty -> Def' ty
cleanup d@Stub{} = d
cleanup d@(Def lhs rhs meqns) = Def lhs rhs meqns' where
  meqns' = eqns <$ guard (not (null eqns))
  eqns = filter needed (fromMaybe [] meqns)
  needed (ps :=: es) = not (null (intersectSet reached (support ps)))
  reached = reachable d

reachable :: Def' ty -> Set String
reachable (Def lhs rhs meqns) = collect (support rhs) (fromMaybe [] meqns)

  where

  reached :: Set String -> Eqn' ty -> Either (Set String) (Eqn' ty)
  reached seen eqn@(ps :=: es) =
    if null (intersectSet seen $ support ps)
      then Right eqn
      else Left (support es)

  collect :: Set String -> [Eqn' ty] -> Set String
  collect seen eqns = case partitionWith (reached seen) eqns of
    (seeing, rest)
      | null seeing -> seen
      | otherwise -> collect (foldl (<>) seen seeing) rest

reachable _ = emptyArr
