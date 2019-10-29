------------------------------------------------------------------------------
-----                                                                    -----
-----     Lnt: Linting Syrup                                             -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Syrup.SRC.Lnt where

import Syrup.SRC.Syn

class Lint t where
  linters :: [t -> [[String]]]
  linters = []

lint :: Lint t => t -> [[String]]
lint t = foldMap ($ t) linters

instance Lint Def where
  linters = [ emptyWhere
            ] where

    emptyWhere = \case
      Def (fun, _) _ (Just []) -> pure $
        [ "Warning: empty where clause in the definition of " ++ fun ++ "."
        , "Did you forget to indent the block of local definitions using spaces?"
        ]
      _ -> []

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

