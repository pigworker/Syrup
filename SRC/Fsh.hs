------------------------------------------------------------------------------
-----                                                                    -----
-----     Fsh: Fresh monad transformer                                   -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Syrup.SRC.Fsh where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

newtype FreshT e m a = FreshT { getFreshT :: StateT e m a }
  deriving (Functor, Applicative, Monad)

type Fresh e = FreshT e Identity

class MonadFresh e m | m -> e where
  fresh :: m e

runFreshT :: Enum e => FreshT e m a -> m (a, e)
runFreshT f = runStateT (getFreshT f) (toEnum 0)

execFreshT :: (Functor m, Enum e) => FreshT e m a -> m e
execFreshT = fmap snd . runFreshT

evalFreshT :: (Functor m, Enum e) => FreshT e m a -> m a
evalFreshT = fmap fst . runFreshT

runFresh :: Enum e => Fresh e a -> (a, e)
runFresh = runIdentity . runFreshT

execFresh :: Enum e => Fresh e a -> e
execFresh = runIdentity . execFreshT

evalFresh :: Enum e => Fresh e a -> a
evalFresh = runIdentity . evalFreshT

instance MonadTrans (FreshT e) where
  lift = FreshT . lift

instance (Enum e, Monad m) => MonadFresh e (FreshT e m) where
  fresh = FreshT $ do
    i <- get
    put (succ i)
    pure i

instance (MonadFresh e m, Monad m) => MonadFresh e (StateT s m) where
  fresh = lift fresh

instance (MonadFresh e m, Monad m, Monoid s) => MonadFresh e (WriterT s m) where
  fresh = lift fresh
