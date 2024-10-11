------------------------------------------------------------------------------
-----                                                                    -----
-----     Lnt: Linting Syrup                                             -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Lnt where

import Data.List (intercalate)

import Language.Syrup.BigArray
import Language.Syrup.Ded
import Language.Syrup.Syn

class Lint t where
  linters :: [t -> [[String]]]
  linters = []

lint :: Lint t => t -> [[String]]
lint t = foldMap ($ t) linters

plural :: [a] -> String -> String -> String
plural [p] x _  = x
plural _   _ xs = xs

instance ty ~ () => Lint (Def' ty) where
  linters = [ emptyWhere
            , deadcode
            , needlessSplits
            ] where

    emptyWhere = \case
      Def (fun, _) _ (Just []) -> pure $
        [ "Warning: empty where clause in the definition of " ++ fun ++ "."
        , "Did you forget to indent the block of local definitions using spaces?"
        ]
      _ -> []

    needlessSplits d = do
      let ps = abstractableCables d
      if null ps then [] else pure
        [ "Warning: the " ++ plural ps "cable " "cables "
          ++ intercalate ", " (show <$> ps)
          ++ plural ps " is" " are"
          ++ " taken apart only to be reconstructed or unused."
        , "Did you consider giving each cable a name without breaking it up?"
        ]

    deadcode d = case foldMapSet pure (unused d) of
      [] -> []
      ns -> pure
        [ "Warning: the " ++ plural ns "wire " "wires "
          ++ intercalate ", " ns
          ++ plural ns " is" " are"
          ++ " defined but never used."
        ]


instance Lint (Source' a) where
  linters = [deflint] where

    deflint = \case
      Definition d -> lint d
      _ -> []

linter :: Lint t
       => [Either [String] (t, String)]
       -> [Either [String] (t, String)]
linter xs = xs >>= \case
  err@Left{}         -> [err]
  src@(Right (t, _)) -> map Left (lint t) ++ [src]



------------------------------------------------------------------------------
-- Needlessly split cables

abstractThisCable :: [Pat] -> Exp -> Bool
abstractThisCable ps e = isEmptyArr (foldMap support ps `intersectSet` go e) where

  cable = Cab () (map patToExp ps)

  go :: Exp -> Set String
  go e | e == cable = mempty
       | otherwise  = case e of
    Var _ x    -> singleton x
    Hol _ x    -> mempty
    App _ _ es -> foldMap go es
    Cab _ es   -> foldMap go es

abstractAnyCable :: Pat -> [Exp] -> [[Pat]]
abstractAnyCable p es = case p of
  PVar{}    -> []
  PCab _ ps ->
    if all (abstractThisCable ps) es
    then [ps]
    else foldMap (`abstractAnyCable` es) ps

abstractableCables :: Def -> [[Pat]]
abstractableCables Stub{} = []
abstractableCables (Def (_, lhs) rhs meqs) =
  let (ps, es) = (lhs, rhs) <> foldMap (foldMap (\ (ps :=: es) -> (ps, es))) meqs in
  foldMap (`abstractAnyCable` es) ps
