------------------------------------------------------------------------------
-----                                                                    -----
-----     Lnt: Linting Syrup                                             -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.Syrup.Lnt where

import Data.Foldable (toList)
import Data.List (intercalate)

import Language.Syrup.BigArray
import Language.Syrup.Ded
import Language.Syrup.Fdk
import Language.Syrup.Pretty
import Language.Syrup.Syn

class Lint t where
  linters :: [t -> [Feedback]]
  linters = []

lint :: Lint t => t -> [Feedback]
lint t = foldMap ($ t) linters

be :: [a] -> String
be [_] = "is"
be _ = "are"

instance Lint (Def' () ()) where
  linters = [ emptyWhere
            , deadcode
            , needlessSplits
            ] where

    emptyWhere = \case
      Def (fun, _) _ (Left True) -> pure $ ALint
        [ "empty where clause in the definition of " ++ fun ++ "."
        , "Did you forget to indent the block of local definitions using spaces?"
        ]
      _ -> []

    needlessSplits d = do
      let ps = abstractableCables d
      if null ps then [] else pure $ ALint
        [ "the " ++ plural ps "cable" "s" ++ " "
          ++ intercalate ", " (basicShow . AList <$> ps)
          ++ " " ++ be ps
          ++ " taken apart only to be reconstructed or unused."
        , "Did you consider giving each cable a name without breaking it up?"
        ]

    deadcode d = case foldMapSet pure (unused d) of
      [] -> []
      ns -> pure $ ALint
        [ "the " ++ plural ns "wire" "s" ++ " "
          ++ intercalate ", " ns
          ++ " " ++ be ns
          ++ " defined but never used."
        ]


instance Lint (Source' a) where
  linters = [deflint] where

    deflint = \case
      Definition d -> lint d
      _ -> []

linter :: Lint t
       => [Either Feedback (t, String)]
       -> [Either Feedback (t, String)]
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
  let (ps, es) = (lhs, toList rhs) <> foldMap (foldMap (\ (ps :=: es) -> (toList ps, toList es))) meqs in
  foldMap (`abstractAnyCable` es) ps
