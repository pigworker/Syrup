------------------------------------------------------------------------------
-----                                                                    -----
-----     Opt: Options for Syrup                                         -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Opt where

import Text.Read (readMaybe)

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
  , experimentLimit :: Maybe Int
  }

defaultOptions :: Options
defaultOptions = Options
  { quiet = False
  , filepath = Nothing
  , graphFormat = RenderedSVG
  , outputFormat = TextOutput
  , experimentLimit = Nothing
  }

defaultMarxOptions :: Options
defaultMarxOptions = Options
  { quiet = False
  , filepath = Nothing
  , graphFormat = SourceDot
  , outputFormat = HTMLOutput
  , experimentLimit = Just 10
  }

parseOptions :: Options -> [String] -> Either String Options
parseOptions acc [] = pure acc
parseOptions acc ("-q" : opts) = parseOptions (acc { quiet = True }) opts
parseOptions acc ("-f" : fp : opts) = parseOptions (acc { filepath = Just fp }) opts
parseOptions acc ("--source-dot" : opts) = parseOptions (acc { graphFormat = SourceDot }) opts
parseOptions acc ("--rendered-svg" : opts) = parseOptions (acc { graphFormat = RenderedSVG }) opts
parseOptions acc ("--html" : opts) = parseOptions (acc { outputFormat = HTMLOutput }) opts
parseOptions acc ("--text" : opts) = parseOptions (acc { outputFormat = TextOutput }) opts
parseOptions acc ("--experiment-limit" : arg : opts) = case readMaybe arg of
  Just k | k >= 0 -> parseOptions (acc { experimentLimit = Just k }) opts
  _ -> Left ("Invalid experiment limit: " ++ show arg ++ ".")
parseOptions acc (opt : opts) = Left ("Unrecognised option " ++ show opt ++ ".")
