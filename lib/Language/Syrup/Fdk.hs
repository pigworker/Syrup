------------------------------------------------------------------------------
-----                                                                    -----
-----     Fdk: Feedback for Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Language.Syrup.Fdk where

import Control.Monad.State (MonadState, get, put, evalState)
import Control.Monad.Writer (MonadWriter, tell)

import Data.List (intercalate)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Utilities.HTML (asHTML, escapeHTML)
import qualified Utilities.HTML as HTML

import Language.Syrup.Opt

data Feedback
  = AnExperiment [String]
  | DotGraph [String]
  | SVGGraph [String]
  | RawCode [String]
  | TruthTable String [String]
  | CircuitDefined String
  | TypeDefined String
  | StubbedOut String
  | FoundHoles String [String]
  | TypeError [String]
  | UnknownIdentifier String
  | MissingImplementation String
  | Ambiguous String [[String]]
  | Undefined String
  | UndefinedType String
  | GenericLog [String] -- TODO: get rid of!

anExperiment :: MonadWriter (Seq Feedback) m => [String] -> m ()
anExperiment ls = tell $ Seq.singleton $ AnExperiment ls

keep :: Options -> Feedback -> Bool
keep opts = \case
  CircuitDefined{} -> not (quiet opts)
  TypeDefined{} -> not (quiet opts)
  StubbedOut{} -> not (quiet opts)
  AnExperiment{} -> True
  RawCode{} -> True
  FoundHoles{} -> True
  DotGraph{} -> True
  SVGGraph{} -> True
  TypeError{} -> True
  TruthTable{} -> True
  UnknownIdentifier{} -> True
  MissingImplementation{} -> True
  Ambiguous{} -> True
  Undefined{} -> True
  UndefinedType{} -> True
  GenericLog{} -> True

fresh :: MonadState Int m => m Int
fresh = do
  n <- get
  let sn = n + 1
  put sn
  pure sn

identifier :: String -> String
identifier = HTML.code . escapeHTML

renderHTML :: MonadState Int m => Feedback -> m String
renderHTML = \case
  AnExperiment ls -> pure $ asHTML ls
  DotGraph ls -> do
    n <- show <$> fresh
    pure $ unlines
      [ "<script type=" ++ show "module" ++ ">"
      , "  import { Graphviz } from \"https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/index.js\";"
      , "  if (Graphviz) {"
      , "    const graphviz = await Graphviz.load();"
      , "    const dot" ++ n ++ " = " ++ show (unlines ls) ++ ";"
      , "    const svg" ++ n ++ " = graphviz.dot(dot" ++ n ++ ");"
      , "    document.getElementById(\"GRAPH" ++ n ++ "\").innerHTML = svg" ++ n ++ ";"
      , "  }"
      , "</script>"
      , "<div id=" ++ show ("GRAPH" ++ n) ++ "></div>"
      ]
  SVGGraph ls -> pure (unlines ls)
  RawCode ls -> pure $ HTML.span ["class=" ++ show "syrup-code"] (unlines $ escapeHTML <$> ls)
  TruthTable x ls -> pure (HTML.pre $ unlines $ map escapeHTML (("Truth table for " ++ x ++ ":") : ls))

  CircuitDefined str -> pure (yay $ "Circuit " ++ identifier str ++ " is defined.")
  TypeDefined str -> pure (yay $ "Type " ++ identifier ("<" ++ str ++ ">") ++ " is defined.")
  StubbedOut nm -> pure (meh $ "Circuit " ++ identifier nm ++ " has been stubbed out.")
  FoundHoles f ls -> pure (meh $ "Found holes in circuit " ++ identifier f ++ ":" ++ HTML.br ++ asHTML ls)
  TypeError ls -> pure (asHTML ls)
  UnknownIdentifier x -> pure (nay $ "I don't know what " ++ identifier x ++ " is.")
  MissingImplementation x -> pure (nay $ "I don't have an implementation for " ++ identifier x ++ ".")
  Ambiguous f zs ->
    pure (nay $ asHTML (("I don't know which of the following is your preferred " ++ f ++ ":") : intercalate [""] zs))
  Undefined f -> pure (nay $ "You haven't defined the circuit " ++ identifier f ++ " just now.")
  UndefinedType x -> pure (nay $ "You haven't defined the type " ++ identifier x ++ " just now.")
  GenericLog ss -> pure (asHTML ss)

  where
    syrupspan txt = HTML.span ["class=" ++ show ("syrup-" ++ txt)]
    yay = syrupspan "happy"
    meh = syrupspan "unimpressed"
    nay = syrupspan "sad"

render :: Feedback -> [String]
render = \case
  AnExperiment ls -> ls
  DotGraph ls -> ls
  SVGGraph ls -> ls
  RawCode ls -> ls
  TruthTable x ls -> ("Truth table for " ++ x ++ ":") : ls
  CircuitDefined str -> ["Circuit " ++ str ++ " is defined."]
  TypeDefined str -> ["Type <" ++ str ++ "> is defined."]
  StubbedOut nm -> ["Circuit " ++ nm ++ " has been stubbed out."]
  FoundHoles f ls -> ("Found holes in circuit " ++ f ++ ":") : ls
  TypeError ls -> ls
  UnknownIdentifier x -> ["I don't know what " ++ x ++ " is."]
  MissingImplementation x -> ["I don't have an implementation for " ++ x ++ "."]
  Ambiguous f zs ->
    ["I don't know which of the following is your preferred " ++ f ++ ":"]
    ++ intercalate [""] zs
  Undefined f -> ["You haven't defined " ++ f ++ " just now."]
  UndefinedType x -> ["You haven't defined the type alias " ++ x ++ " just now."]
  GenericLog ss -> ss

feedback :: Options -> [Feedback] -> [String]
feedback opts = (. filter (keep opts)) $ case outputFormat opts of
  TextOutput -> concatMap ((++ [""]) . render)
  HTMLOutput -> (headerHTML:) . map (++ HTML.br) . flip evalState 0 . traverse renderHTML

  where
  headerHTML = unlines
    [ "<style>"
    , "  .syrup-code {"
    , "    display: block;"
    , "    font-family: monospace;"
    , "    font-size: 17px;"
    , "    white-space: pre;"
    , "    margin: 1em 0;"
    , "  }"
    , "  .syrup-happy:before {"
    , "    content: \"\\2705\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "  .syrup-sad:before {"
    , "    content: \"\\274C\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "  .syrup-unimpressed:before {"
    , "    content: \"\\26A0\\FE0F\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "</style>"
    ]
