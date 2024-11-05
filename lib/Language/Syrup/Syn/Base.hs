------------------------------------------------------------------------------
-----                                                                    -----
-----     Syn: Syntax for Syrup (basic types)                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Syrup.Syn.Base where

import Control.Monad (guard)

------------------------------------------------------------------------------
-- Values

data Va
  = V0 | V1 | VQ | VC [Va]
  deriving (Eq, Ord)

instance Show Va where
  show V0 = "0"
  show V1 = "1"
  show VQ = "?"
  show (VC vs) = "[" ++ foldMap show vs ++ "]"

------------------------------------------------------------------------------
-- Circuit configuration

data CircuitConfig = CircuitConfig
  { memoryConfig :: [Va]
  , valuesConfig :: [Va]
  } deriving Show

circuitConfig :: Bool -> CircuitConfig -> String
circuitConfig isLHS (CircuitConfig mems vals) = concat $
  (concat [ "{", foldMap show mems, "}" ] <$ guard (not $ null mems))
  ++ ("(" <$ guard isLHS)
  ++ map show vals
  ++ (")" <$ guard isLHS)

------------------------------------------------------------------------------
-- Types

data Ty t x
  = TyV x
  | Bit t
  | Cable [Ty t x]
  deriving (Eq, Functor, Foldable, Traversable)


-- using this rather than () because we want a
-- pretty Unit = ""   instance rather than the
-- pretty ()   = "()" one
data Unit = Unit deriving (Eq)

------------------------------------------------------------------------------
-- Phases

data Ti = T0 | T1 deriving (Show, Eq)
