------------------------------------------------------------------------------
-----                                                                    -----
-----     Opt: Options for Syrup                                         -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Opt where

import Data.Set

data GraphFormat
  = RenderedSVG
  | SourceDot

data Options = Options
  { quiet :: Bool
  , filepath :: Maybe FilePath
  , graphFormat :: GraphFormat
  }

defaultOptions :: Options
defaultOptions = Options
  { quiet = False
  , filepath = Nothing
  , graphFormat = RenderedSVG
  }

defaulMarxOptions :: Options
defaulMarxOptions = Options
  { quiet = False
  , filepath = Nothing
  , graphFormat = SourceDot
  }

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions where

  go acc [] = pure acc
  go acc ("-q" : opts) = go (acc { quiet = True }) opts
  go acc ("-f" : fp : opts) = go (acc { filepath = Just fp }) opts
  go acc ("--source-dot" : opts) = go (acc { graphFormat = SourceDot }) opts
  go acc ("--rendered-svg" : opts) = go (acc { graphFormat = RenderedSVG }) opts
  go acc (opt : opts) = Left ("Unrecognised option \"" ++ opt ++ "\".")
