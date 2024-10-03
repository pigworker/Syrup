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

data OutputFormat
  = TextOutput
  | HTMLOutput

data Options = Options
  { quiet :: Bool
  , filepath :: Maybe FilePath
  , graphFormat :: GraphFormat
  , outputFormat :: OutputFormat
  }

defaultOptions :: Options
defaultOptions = Options
  { quiet = False
  , filepath = Nothing
  , graphFormat = RenderedSVG
  , outputFormat = TextOutput
  }

defaulMarxOptions :: Options
defaulMarxOptions = Options
  { quiet = False
  , filepath = Nothing
  , graphFormat = SourceDot
  , outputFormat = HTMLOutput
  }

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions where

  go acc [] = pure acc
  go acc ("-q" : opts) = go (acc { quiet = True }) opts
  go acc ("-f" : fp : opts) = go (acc { filepath = Just fp }) opts
  go acc ("--source-dot" : opts) = go (acc { graphFormat = SourceDot }) opts
  go acc ("--rendered-svg" : opts) = go (acc { graphFormat = RenderedSVG }) opts
  go acc ("--html" : opts) = go (acc { outputFormat = HTMLOutput }) opts
  go acc ("--text" : opts) = go (acc { outputFormat = TextOutput }) opts
  go acc (opt : opts) = Left ("Unrecognised option \"" ++ opt ++ "\".")
