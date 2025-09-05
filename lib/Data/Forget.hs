{-# LANGUAGE FlexibleInstances     #-}

module Data.Forget where

import Data.Void (Void)
import Unsafe.Coerce

class Forget a b where
instance Forget Void a where

instance Forget a b => Forget (Maybe a) (Maybe b) where
instance Forget a b => Forget [a] [b] where

forget :: Forget a b => a -> b
forget = unsafeCoerce
