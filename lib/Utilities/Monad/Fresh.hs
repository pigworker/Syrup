{-# LANGUAGE RankNTypes #-}

module Utilities.Monad.Fresh where

import Control.Monad.State (MonadState, gets, evalState)
import Utilities.Lens

type MonadFresh s m =
  ( Has Fresh s
  , MonadState s m
  )

data Fresh
  = Fresh
  { freshPrefix :: String
  , freshSuffix :: Int
  }

fresh :: MonadFresh s m => m Fresh
fresh = do
  nm@(Fresh pref oldi) <- gets (^. hasLens)
  let newi = Fresh pref (1 + oldi)
  hasLens .= newi
  pure nm

renderFresh :: Fresh -> String
renderFresh (Fresh pref i) = pref ++ "_" ++ show i

instance Has Fresh Fresh where
  hasLens i = i

evalFresh :: String -> (forall s m. MonadFresh s m => m a) -> a
evalFresh pref k = evalState k (Fresh pref 0)
