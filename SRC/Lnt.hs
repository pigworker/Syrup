------------------------------------------------------------------------------
-----                                                                    -----
-----     Lnt: Linting Syrup                                             -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Syrup.SRC.Lnt where

import Syrup.SRC.Syn

class Lint t where
  lint :: t -> Maybe [String]
  lint _ = Nothing

instance Lint Def where
  lint = \case
    Def (fun, _) _ (Just []) -> Just $
      [ "Warning: empty where clause in the definition of " ++ fun ++ "."
      , "Did you forget to indent the block of local definitions using spaces?"
      ]
    _ -> Nothing

instance Lint (Source' a) where
  lint = \case
    Definition d -> lint d
    _ -> Nothing

linter :: Lint t
       => [Either [String] (t, String)]
       -> [Either [String] (t, String)]
linter xs = xs >>= \case
  err@Left{}         -> [err]
  src@(Right (t, _)) -> maybe id ((:) . Left) (lint t) [src]

