{-# LANGUAGE StandaloneDeriving #-}

module Data.IMaybe where

import Data.Kind (Type)

data IMaybe (b :: Bool) (a :: Type) :: Type where
  IJust :: a -> IMaybe True a
  INothing :: IMaybe False a

deriving instance Show a => Show (IMaybe b a)
deriving instance Functor (IMaybe b)

fromIJust :: IMaybe True a -> a
fromIJust (IJust x) = x
