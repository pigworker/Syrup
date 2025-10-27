------------------------------------------------------------------------------
-----                                                                    -----
-----     Syn: Syntax for Syrup (basic types)                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Syrup.Syn.Base where

import Control.Monad (ap, guard)

import Data.Forget (Forget)
import Data.String (IsString(..))
import Data.Void (Void)

import Language.Syrup.BigArray (Set)

------------------------------------------------------------------------------
-- Names

newtype TyName = TyName { getTyName :: String } deriving (Show, Eq, Ord)

newtype Name = Name { getName :: String } deriving (Show, Eq, Ord)
instance IsString Name where
  fromString = Name


type Names = Set Name


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
  = Meta x
  | TVar TyName (Ty t Void) -- type aliases are closed
  | Bit t
  | Cable [Ty t x]
  deriving (Eq, Functor, Foldable, Traversable)

instance Forget b c => Forget (Ty a b) (Ty a c) where

isBit :: Ty t x -> Maybe t
isBit (Bit a) = Just a
isBit _ = Nothing

-- boring instances

instance Monad (Ty t) where
  return = Meta
  Meta x   >>= k = k x
  TVar s t >>= _ = TVar s t
  Bit t    >>= _ = Bit t
  Cable ts >>= k = Cable (fmap (>>= k) ts)

instance Applicative (Ty t) where
  pure = return
  (<*>) = ap


-- using this rather than () because we want a
-- pretty Unit = ""   instance rather than the
-- pretty ()   = "()" one
data Unit = Unit deriving (Eq)

------------------------------------------------------------------------------
-- Phases

data Ti = T0 | T1 deriving (Show, Eq)
