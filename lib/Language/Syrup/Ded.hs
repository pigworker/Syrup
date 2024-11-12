------------------------------------------------------------------------------
-----                                                                    -----
-----     Ded: Dead code elimination for Syrup                           -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Ded where

import Language.Syrup.BigArray
import Language.Syrup.Syn
import Language.Syrup.Utils

cleanup :: Def' tys ty -> Def' tys ty
cleanup d@Stub{} = d
cleanup d@(Def lhs rhs meqns) = Def lhs rhs (toEqns eqns) where
  eqns = filter needed (fromEqns meqns)
  needed (ps :=: es) = not (null (intersectSet reached (support ps)))
  reached = reachable d

unused :: Def' tys ty -> Set String
unused Stub{} = emptyArr
unused d@(Def lhs rhs meqns) =
  let reached = reachable d in
  diffSet (support $ map (\ (ps :=: _) -> ps) (fromEqns meqns)) reached


reachable :: Def' tys ty -> Set String
reachable Stub{} = emptyArr
reachable (Def lhs rhs meqns) = collect (support rhs) (fromEqns meqns)

  where

  reached :: Set String -> Eqn' tys ty -> Either (Set String) (Eqn' tys ty)
  reached seen eqn@(ps :=: es) =
    if null (intersectSet seen $ support ps)
      then Right eqn
      else Left (support es)

  collect :: Set String -> [Eqn' tys ty] -> Set String
  collect seen eqns = case partitionWith (reached seen) eqns of
    (seeing, rest)
      | null seeing -> seen
      | otherwise -> collect (foldl (<>) seen seeing) rest
