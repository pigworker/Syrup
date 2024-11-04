{-# LANGUAGE ScopedTypeVariables #-}

module Utilities.Vector where

import Data.Kind
import Utilities.Nat

infixr 5 :*
data Vector (n :: Nat) (a :: Type) where
  VNil :: Vector Ze a
  (:*) :: a -> Vector n a -> Vector (Su n) a

hd :: Vector (Su n) a -> a
hd (t :* _) = t

instance Functor (Vector n) where
  fmap f VNil = VNil
  fmap f (x :* xs) = f x :* fmap f xs

instance Foldable (Vector n) where
  foldMap f VNil = mempty
  foldMap f (x :* xs) = f x <> foldMap f xs

instance Traversable (Vector n) where
  traverse f VNil = pure VNil
  traverse f (x :* xs) = (:*) <$> f x <*> traverse f xs

data AList a where
  AList :: forall n a. Vector n a -> AList a

nil :: AList a
nil = AList VNil

cons :: a -> AList a -> AList a
cons x (AList xs) = AList (x :* xs)

fromList :: [a] -> AList a
fromList = foldr cons nil
