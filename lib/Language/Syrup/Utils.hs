------------------------------------------------------------------------------
-----                                                                    -----
-----     Utils: Syrup Utility functions                                 -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Utils where

import Control.Arrow
import Data.Bifunctor as Bi
import Data.Maybe

isNothing :: Maybe a -> Bool
isNothing = \case
  Nothing -> True
  Just{}  -> False

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

padRight :: Int -> String -> String
padRight n xs
  | n <= 0    = xs
  | otherwise = xs ++ replicate n ' '

unzipWith :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith f []     = ([], [])
unzipWith f (a:as) =
  let (b , c)  = f a
      (bs, cs) = unzipWith f as
  in (b:bs, c:cs)


partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith p [] = ([], [])
partitionWith p (x : xs)
  = either (Bi.first . (:)) (Bi.second . (:)) (p x)
  $ partitionWith p xs
