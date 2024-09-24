{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Utilities.Lens where

import Control.Applicative (Const(Const), getConst)
import Control.Monad.Identity (Identity(Identity), runIdentity)
import Control.Monad.State (MonadState, get, modify)

type Lens i o = forall f. Functor f => (i -> f i) -> (o -> f o)

(^.) :: o -> Lens i o -> i
v ^. l = getConst (l Const v)

over :: Lens i o -> (i -> i) -> (o -> o)
over l f = runIdentity . l (Identity . f)

(.=) :: MonadState o m => Lens i o -> i -> m ()
l .= v = l %= const v

(%=) :: MonadState o m => Lens i o -> (i -> i) -> m ()
l %= f = modify (over l f)

use :: MonadState o m => Lens i o -> m i
use l = getConst . l Const <$> get

class Has i o where
  hasLens :: Lens i o
