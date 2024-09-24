------------------------------------------------------------------------------
-----                                                                    -----
-----     Opt: Options for Syrup                                         -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Opt where

import Data.Set

data Options = Options
  { quiet :: Bool
  , filepath :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { quiet = False
  , filepath = Nothing
  }

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions where

  go acc [] = pure acc
  go acc ("-q" : opts) = go (acc { quiet = True }) opts
  go acc ("-f" : fp : opts) = go (acc { filepath = Just fp }) opts
  go acc (opt : opts) = Left ("Unrecognised option \"" ++ opt ++ "\".")
