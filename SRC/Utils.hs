------------------------------------------------------------------------------
-----                                                                    -----
-----     Utils: Syrup Utility functions                                 -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Syrup.SRC.Utils where

import Control.Arrow

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe p = go where

  go []             = ([], [])
  go aas@(a : as)
    | Just b <- p a = ((b :) *** id) (go as)
    | otherwise     = ([], aas)

isLeft :: Either a b -> Maybe a
isLeft = \case
  Left a -> Just a
  _      -> Nothing

unzipWith :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith f = \case
  []       -> ([], [])
  (a : as) ->
    let (b , c)  = f a
        (bs, cs) = unzipWith f as
    in (b : bs, c : cs)
