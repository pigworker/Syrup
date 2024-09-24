------------------------------------------------------------------------------
-----                                                                    -----
-----     Opt: Options for Syrup                                         -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Opt where

import Data.Set

data Option
  = Quiet
  deriving (Eq, Ord)

type Options = Set Option

addOption :: String -> Options -> Maybe Options
addOption "-q" opts = pure (insert Quiet opts)
addOption _ _ = Nothing

parseOptions :: [String] -> Either String Options
parseOptions = go mempty where

  go acc [] = pure acc
  go acc (opt : opts) = case addOption opt acc of
    Nothing -> Left ("Unrecognised option \"" ++ opt ++ "\".")
    Just acc' -> go acc' opts
