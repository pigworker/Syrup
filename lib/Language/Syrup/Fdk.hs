------------------------------------------------------------------------------
-----                                                                    -----
-----     Fdk: Feedback for Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Language.Syrup.Fdk where

import Control.Monad.Writer (MonadWriter, tell)

import Data.List (intercalate)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Language.Syrup.Opt

data Feedback
  = AnExperiment [String]
  | CircuitDefined String
  | TypeDefined String
  | StubbedOut String
  | TypeError [String]
  | UnknownIdentifier String
  | MissingImplementation String
  | AWarning [String]
  | Ambiguous String [[String]]
  | Undefined String
  | GenericLog [String] -- TODO: get rid of!

anExperiment :: MonadWriter (Seq Feedback) m => [String] -> m ()
anExperiment ls = tell $ Seq.singleton $ AnExperiment ls

keep :: Options -> Feedback -> Bool
keep opts = \case
  CircuitDefined{} -> not (quiet opts)
  TypeDefined{} -> not (quiet opts)
  StubbedOut{} -> not (quiet opts)
  AnExperiment{} -> True
  TypeError{} -> True
  UnknownIdentifier{} -> True
  MissingImplementation{} -> True
  AWarning{} -> True
  Ambiguous{} -> True
  Undefined{} -> True
  GenericLog{} -> True

render :: Feedback -> [String]
render = \case
  AnExperiment ls -> ls
  CircuitDefined str -> [str ++ " is defined."]
  TypeDefined str -> ["Type alias " ++ str ++ " is defined."]
  StubbedOut nm -> [nm ++ " has been stubbed out."]
  TypeError ls -> ls
  UnknownIdentifier x -> ["I don't know what " ++ x ++ " is."]
  MissingImplementation x -> ["I don't have an implementation for " ++ x ++ "."]
  AWarning ls -> ls
  Ambiguous f zs ->
    ["I don't know which of the following is your preferred " ++ f ++ ":"]
    ++ intercalate [""] zs
  Undefined f -> ["You haven't defined " ++ f ++ " just now."]
  GenericLog ss -> ss

feedback :: Options -> [Feedback] -> [String]
feedback opts = concatMap ((++ [""]) . render) . filter (keep opts)
