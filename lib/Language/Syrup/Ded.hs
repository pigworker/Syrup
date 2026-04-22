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

cleanup :: Def' Name ty -> Def' Name ty
cleanup d@Stub{} = d
cleanup d@(Def lhs rhs meqns) = Def lhs rhs meqns' where
  meqns' = eqns <$ guard (not (null eqns))
  eqns = filter needed (fromMaybe [] meqns)
  needed (ps :=: es) = not (null (intersectSet reached (support ps)))
  reached = reachable d

unused :: Def' Name ty -> Set String
unused Stub{} = emptyArr
unused d@(Def lhs rhs meqns) =
  let reached = reachable d in
  diffSet (support $ map (\ (ps :=: _) -> ps) (fromMaybe [] meqns)) reached


reachable :: Def' Name ty -> Set String
reachable Stub{} = emptyArr
reachable (Def lhs rhs meqns) = collect (support rhs) (fromMaybe [] meqns)

  where

  reached :: Set String -> Eqn' Name ty -> Either (Set String) (Eqn' Name ty)
  reached seen eqn@(ps :=: es) =
    if null (intersectSet seen $ support ps)
      then Right eqn
      else Left (support es)

  collect :: Set String -> [Eqn' Name ty] -> Set String
  collect seen eqns = case partitionWith (reached seen) eqns of
    (seeing, rest)
      | null seeing -> seen
      | otherwise -> collect (foldl (<>) seen seeing) rest
