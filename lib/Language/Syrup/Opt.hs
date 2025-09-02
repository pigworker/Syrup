------------------------------------------------------------------------------
-----                                                                    -----
-----     Opt: Options for Syrup                                         -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Opt where

data GraphFormat
  = RenderedSVG
  | SourceDot

data OutputFormat
  = TextOutput
  | HtmlOutput

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

defaultMarxOptions :: Options
defaultMarxOptions = Options
  { quiet = False
  , filepath = Nothing
  , graphFormat = SourceDot
  , outputFormat = HtmlOutput
  }

parseOptions :: Options -> [String] -> Either String Options
parseOptions acc [] = pure acc
parseOptions acc ("-q" : opts) = parseOptions (acc { quiet = True }) opts
parseOptions acc ("-f" : fp : opts) = parseOptions (acc { filepath = Just fp }) opts
parseOptions acc ("--source-dot" : opts) = parseOptions (acc { graphFormat = SourceDot }) opts
parseOptions acc ("--rendered-svg" : opts) = parseOptions (acc { graphFormat = RenderedSVG }) opts
parseOptions acc ("--html" : opts) = parseOptions (acc { outputFormat = HtmlOutput }) opts
parseOptions acc ("--text" : opts) = parseOptions (acc { outputFormat = TextOutput }) opts
parseOptions acc (opt : opts) = Left ("Unrecognised option \"" ++ opt ++ "\".")
