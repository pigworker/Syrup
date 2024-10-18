module Utilities.Bwd where

data Bwd a = Lin | (:<) (Bwd a) a
  deriving (Eq, Show, Functor, Foldable, Traversable)

(<><) :: Bwd a -> [a] -> Bwd a
sx <>< [] = sx
sx <>< (x : xs) = (sx :< x) <>< xs
