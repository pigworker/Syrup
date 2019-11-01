------------------------------------------------------------------------------
-----                                                                    -----
-----     Utils: Syrup Utility functions                                 -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Syrup.SRC.Utils where

import Control.Arrow
import Data.Bifunctor as Bi
import Data.Maybe

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

isRight :: Either a b -> Maybe b
isRight = \case
  Right b -> Just b
  _       -> Nothing

allLeftsOrRight :: [Either a b] -> Either [a] [b]
allLeftsOrRight []             = Left []
allLeftsOrRight (Left a : rs)  = Bi.first (a :) (allLeftsOrRight rs)
allLeftsOrRight (Right b : rs) = Right (b : mapMaybe isRight rs)
