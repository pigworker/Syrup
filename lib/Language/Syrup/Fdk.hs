------------------------------------------------------------------------------
-----                                                                    -----
-----     Fdk: Feedback for Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Syrup.Fdk where

import Control.Monad.State (MonadState, get, put, evalState)
import Control.Monad.Writer (MonadWriter, tell)

import Data.Kind (Type)
import Data.List (intercalate, intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Utilities.HTML (asHTML, escapeHTML)
import qualified Utilities.HTML as HTML

import Language.Syrup.BigArray (Set, isEmptyArr, foldMapSet)
import Language.Syrup.Opt (Options, quiet)

------------------------------------------------------------------------------
-- Feedback classes

type MonadRenderHTML m =
  ( MonadState Int m
  )

class Categorise t where
  categorise :: t -> FeedbackStatus

class Render t where
  type Builder t :: Type
  render :: t -> String
  renderHTML :: MonadRenderHTML m => t -> m (Builder t)

plural :: [a] -> String -> String -> String
plural [_] str _ = str
plural _   str s = str ++ s

------------------------------------------------------------------------------
-- Feedback status

data FeedbackStatus
  = Success
  | Comment
  | Warning
  | Error
  deriving Eq

instance Semigroup FeedbackStatus where
  Success <> f = f
  e <> Success = e
  Comment <> f = f
  e <> Comment = e
  Warning <> f = f
  e <> Warning = e
  _ <> _       = Error

instance Render FeedbackStatus where
  type Builder FeedbackStatus = String -> String

  render = \case
    Success -> ""
    Comment -> ""
    Warning -> "Warning"
    Error -> "Error"

  renderHTML fcl = pure $ HTML.span ["class=" ++ show ("syrup-" ++ feedbackClass fcl)] where
    feedbackClass = \case
      Success -> "happy"
      Comment -> "comment"
      Warning -> "warning"
      Error -> "error"

------------------------------------------------------------------------------
-- Scope errors

type Name  = String
type Names = Set Name

data ScopeLevel = Local | Global
  deriving (Eq)

levelMsg :: ScopeLevel -> String
levelMsg = \case
  Local  -> "local"
  Global -> "top-level"

data ScopeError
  = OutOfScope ScopeLevel Name Names
    -- name that cannot be resolved & suggestions
  | Shadowing  ScopeLevel Names
    -- shadowing an existing variable

instance Categorise ScopeError where
  categorise = \case
    OutOfScope{}       -> Error
    Shadowing Local _  -> Error
    Shadowing Global _ -> Warning

metaRender :: (String -> String) -> ScopeError -> String
metaRender f e = concat $ case e of
    OutOfScope l n ns ->
      let names = foldMapSet (pure . f) ns in
      "You tried to use "
      : n
      : " but it is not in scope."
      : if isEmptyArr ns then [] else
        "\n"
        : plural names "Did you mean" " one of these"
        : ": "
        : intersperse ", " names
        ++ ["?"]
    Shadowing l ns ->
      let names = foldMapSet (pure . f) ns in
      "You are redefining the "
      : levelMsg l
      : " " : plural names "variable" "s"
      : " " : intersperse ", " names
      ++ ["."]


instance Render ScopeError where
  type Builder ScopeError = String

  render = metaRender id
  renderHTML = pure . metaRender HTML.code

data Feedback
  -- errors
  = ANoExecutable String
  | AScopeError ScopeError
  | ASyntaxError [String]
  | ATypeError [String]
  | AnAmbiguousDefinition String [[String]]
  | AnUndefinedCircuit String
  | AnUndefinedType String
  | AnUnknownIdentifier String

  -- warnings
  | AMissingImplementation String
  | AStubbedOut String

  -- comments
  | ACircuitDefined String
  | ALint [String]
  | ATypeDefined String

  -- successes
  | ADotGraph [String]
  | ARawCode [String]
  | ATruthTable String [String]
  | AnExperiment [String]
  | AnSVGGraph [String]


  | GenericLog [String] -- TODO: get rid of!

instance Categorise Feedback where
  categorise = \case
    -- errors
    ANoExecutable{} -> Error
    AScopeError{} -> Error
    ASyntaxError{} -> Error
    ATypeError{} -> Error
    AnAmbiguousDefinition{} -> Error
    AnUndefinedCircuit{} -> Error
    AnUndefinedType{} -> Error
    AnUnknownIdentifier{} -> Error

    -- warnings
    AMissingImplementation{} -> Warning
    AStubbedOut{} -> Warning

    -- comments
    ACircuitDefined{} -> Comment
    ALint{} -> Comment
    ATypeDefined{} -> Comment

    -- successes
    ADotGraph{} -> Success
    ARawCode{} -> Success
    ATruthTable{} -> Success
    AnExperiment{} -> Success
    AnSVGGraph{} -> Success


anExperiment :: MonadWriter (Seq Feedback) m => [String] -> m ()
anExperiment ls = tell $ Seq.singleton $ AnExperiment ls

keep :: Options -> Feedback -> Bool
keep opts fdk
  = not (quiet opts)
  && categorise fdk /= Comment
  && case fdk of { AStubbedOut{} -> False; _ -> True }

fresh :: MonadState Int m => m Int
fresh = do
  n <- get
  let sn = n + 1
  put sn
  pure sn

identifier :: String -> String
identifier = HTML.code . escapeHTML

instance Render Feedback where
  type Builder Feedback = String
  render = undefined
  renderHTML = undefined

{-
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
  Lint ls -> pure (meh $ asHTML ls)
  NoExecutable exe -> pure (meh ("Could not find the " ++ identifier exe ++ " executable :("))
  SVGGraph ls -> pure (unlines ls)
  RawCode ls -> pure $ HTML.span ["class=" ++ show "syrup-code"] (unlines $ escapeHTML <$> ls)
  TruthTable x ls -> pure (HTML.pre $ unlines $ map escapeHTML (("Truth table for " ++ x ++ ":") : ls))
  SyntaxError ls -> pure (nay $ asHTML ls)
  ScopeError ls -> pure (nay $ asHTML ls)
  CircuitDefined str -> pure (yay $ "Circuit " ++ identifier str ++ " is defined.")
  TypeDefined str -> pure (yay $ "Type " ++ identifier ("<" ++ str ++ ">") ++ " is defined.")
  StubbedOut nm -> pure (meh $ "Circuit " ++ identifier nm ++ " has been stubbed out.")
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
  Lint ls -> ls
  DotGraph ls -> ls
  SVGGraph ls -> ls
  RawCode ls -> ls
  SyntaxError ls -> ls
  ScopeError ls -> ls
  TruthTable x ls -> ("Truth table for " ++ x ++ ":") : ls
  CircuitDefined str -> ["Circuit " ++ str ++ " is defined."]
  TypeDefined str -> ["Type <" ++ str ++ "> is defined."]
  StubbedOut nm -> ["Circuit " ++ nm ++ " has been stubbed out."]
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
    , "  .syrup-comment:before {"
    , "    content: \" \";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "  .syrup-happy:before {"
    , "    content: \"\\2705\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "  .syrup-error:before {"
    , "    content: \"\\274C\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "  .syrup-warning:before {"
    , "    content: \"\\26A0\\FE0F\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "</style>"
    ]
-}
