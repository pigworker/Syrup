------------------------------------------------------------------------------
-----                                                                    -----
-----     Fdk: Feedback for Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Language.Syrup.Fdk where

import Control.Monad.State (MonadState, get, put, evalState)
import Control.Monad.Writer (MonadWriter, tell)

import Data.List (intercalate, intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Utilities.HTML (asHTML, escapeHTML)
import qualified Utilities.HTML as HTML

import Language.Syrup.BigArray (Set, isEmptyArr, foldMapSet)
import Language.Syrup.Opt (Options(..), quiet, OutputFormat(..))

------------------------------------------------------------------------------
-- Feedback classes

type MonadRenderHTML m =
  ( MonadState Int m
  )

class Categorise t where
  categorise :: t -> FeedbackStatus

class Render t where
  render :: t -> [String]
  renderHTML :: MonadRenderHTML m => t -> m String

instance Render t => Render [t] where
  render = concatMap ((++ [""]) . render)
  renderHTML = fmap (intercalate HTML.br) . traverse renderHTML

plural :: [a] -> String -> String -> String
plural []  str _ = str
plural [_] str _ = str
plural _   str s = str ++ s

------------------------------------------------------------------------------
-- Feedback status

data FeedbackStatus
  = Success
  | Comment
  | Warning
  | Error
  | Internal
  deriving Eq

isErroring :: FeedbackStatus -> Bool
isErroring = \case
  Success  -> False
  Comment  -> False
  Warning  -> False
  Error    -> True
  Internal -> True

instance Semigroup FeedbackStatus where
  Success <> f = f
  e <> Success = e
  Comment <> f = f
  e <> Comment = e
  Warning <> f = f
  e <> Warning = e
  Error <> f   = f
  e <> Error   = e
  _ <> _       = Internal

instance Monoid FeedbackStatus where
  mempty = Success
  mappend = (<>)

instance Render FeedbackStatus where
  render = pure . \case
    Success -> ""
    Comment -> ""
    Warning -> "Warning"
    Error -> "Error"
    Internal -> "Internal error"

  renderHTML fcl = pure $ HTML.span ["class=" ++ show ("syrup-" ++ feedbackClass fcl)] "" where
    feedbackClass = \case
      Success -> "happy"
      Comment -> "comment"
      Warning -> "warning"
      Error -> "error"
      Internal -> "internal"

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
  render = pure . metaRender id
  renderHTML = pure . metaRender identifier

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
  || (categorise fdk /= Comment
  && case fdk of { AStubbedOut{} -> False; _ -> True })

fresh :: MonadState Int m => m Int
fresh = do
  n <- get
  let sn = n + 1
  put sn
  pure sn

identifier :: String -> String
identifier = HTML.code . escapeHTML

instance Render Feedback where
  render e =
    let preamb = concat (render $ categorise e) in
    prepend (plural preamb preamb ": ") (go e) where

    prepend :: Semigroup a => a -> [a] -> [a]
    prepend x []       = [x]
    prepend x (y : xs) = (x <> y : xs)

    go = \case
      AnExperiment ls -> ls
      ALint ls -> ls
      ADotGraph ls -> ls
      AnSVGGraph ls -> ls
      ARawCode ls -> ls
      ASyntaxError ls -> ls
      AScopeError ls -> render ls
      ANoExecutable exe -> ["Could not find the " ++ exe ++ " executable :("]
      ATruthTable x ls -> ("Truth table for " ++ x ++ ":") : ls
      ACircuitDefined str -> ["Circuit " ++ str ++ " is defined."]
      ATypeDefined str -> ["Type <" ++ str ++ "> is defined."]
      AStubbedOut nm -> ["Circuit " ++ nm ++ " has been stubbed out."]
      ATypeError ls -> ls
      AnUnknownIdentifier x -> ["I don't know what " ++ x ++ " is."]
      AMissingImplementation x -> ["I don't have an implementation for " ++ x ++ "."]
      AnAmbiguousDefinition f zs ->
        ["I don't know which of the following is your preferred " ++ f ++ ":"]
        ++ intercalate [""] zs
      AnUndefinedCircuit f -> ["You haven't defined the circuit " ++ f ++ " just now."]
      AnUndefinedType x -> ["You haven't defined the type alias " ++ x ++ " just now."]

  renderHTML e = do
    cat <- renderHTML (categorise e)
    msg <- goHTML e
    pure (unlines [cat, msg])


    where

    goHTML = \case
      AnExperiment ls -> pure $ asHTML ls
      ADotGraph ls -> do
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
      ALint ls -> pure (asHTML ls)
      ANoExecutable exe -> pure ("Could not find the " ++ identifier exe ++ " executable :(")
      AnSVGGraph ls -> pure (unlines ls)
      ARawCode ls -> pure $ HTML.span ["class=" ++ show "syrup-code"] (unlines $ escapeHTML <$> ls)
      ATruthTable x ls -> pure (HTML.pre $ unlines $ map escapeHTML (("Truth table for " ++ x ++ ":") : ls))
      ASyntaxError ls -> pure (asHTML ls)
      AScopeError ls -> renderHTML ls
      ACircuitDefined str -> pure ("Circuit " ++ identifier str ++ " is defined.")
      ATypeDefined str -> pure ("Type " ++ identifier ("<" ++ str ++ ">") ++ " is defined.")
      AStubbedOut nm -> pure ("Circuit " ++ identifier nm ++ " has been stubbed out.")
      ATypeError ls -> pure (asHTML ls)
      AnUnknownIdentifier x -> pure ("I don't know what " ++ identifier x ++ " is.")
      AMissingImplementation x -> pure ("I don't have an implementation for " ++ identifier x ++ ".")
      AnAmbiguousDefinition f zs ->
        pure (asHTML (("I don't know which of the following is your preferred " ++ f ++ ":") : intercalate [""] zs))
      AnUndefinedCircuit f -> pure ("You haven't defined the circuit " ++ identifier f ++ " just now.")
      AnUndefinedType x -> pure ("You haven't defined the type " ++ identifier x ++ " just now.")

feedback :: Options -> [Feedback] -> [String]
feedback opts = (. filter (keep opts)) $ case outputFormat opts of
  TextOutput -> render
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
    , "  .syrup-comment:before {"
    , "    content: \" \";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "  .syrup-warning:before {"
    , "    content: \"\\26A0\\FE0F\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "  .syrup-error:before {"
    , "    content: \"\\274C\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "  .syrup-internal:before {"
    , "    content: \"\\1F480\";"
    , "    padding: 0 6px 0 0;"
    , "  }"
    , "</style>"
    ]
