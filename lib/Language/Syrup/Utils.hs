------------------------------------------------------------------------------
-----                                                                    -----
-----     Utils: Syrup Utility functions                                 -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.Syrup.Utils where

import Control.Arrow ((***))

import qualified Data.Bifunctor as Bi
import Data.Foldable (fold)
import Data.Maybe (mapMaybe)
import Data.String (IsString)

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

plural :: Monoid s => [a] -> s -> s -> s
plural (_ : _ : _) str s = str <> s
plural _ str _ = str

oxfordList :: (Monoid a, IsString a) => [a] -> a
oxfordList [] = ""
oxfordList [x] = x
oxfordList [x,y] = fold [x, " and ", y]
oxfordList xs = fold (go xs) where

  go = \case
    [] -> []
    [x] -> [x]
    [x,y] -> [x, ", and ", y]
    (x:xs) -> x : ", " : go xs

be :: IsString d => [a] -> d
be [_] = "is"
be _ = "are"

($$) :: Monoid m => (m -> a) -> [m] -> a
f $$ x = f (fold x)
