------------------------------------------------------------------------------
-----                                                                    -----
-----     Fdk: Feedback for Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.Syrup.Fdk where

import Prelude hiding (div, id, unlines, unwords)

import Control.Monad.State (MonadState, get, put, evalState)
import Control.Monad.Writer (MonadWriter, tell)

import Data.Foldable (fold)
import Data.List (intercalate, intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Void (Void, absurd)

import Language.Syrup.BigArray (isEmptyArr, foldMapSet)
import Language.Syrup.Doc
import Language.Syrup.Pretty
import Language.Syrup.Opt (Options(..), quiet)
import Language.Syrup.Syn.Base

import Language.Syrup.Utils (be, plural, oxfordList)

{-
import Text.Blaze.Html5
  (AttributeValue, Html, (!), br, code, div, pre, preEscapedString, toHtml, toValue)
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes
  (class_, id, style, type_)
-}

($$) :: Monoid m => (m -> a) -> [m] -> a
f $$ x = f (fold x)

{-
($$$) :: (Html -> a) -> [Html] -> a
f $$$ x = f $$ intersperse "\n" x
-}


{-
metaRender :: (IsString a, Monoid a) => (Name -> a) -> ScopeError -> a
metaRender f e = fold $ case e of
    OutOfScope l n ns ->
      let names = foldMapSet (pure . f) ns in
      "You tried to use "
      : f n
      : " but it is not in scope."
      : if isEmptyArr ns then [] else
        [ "\n"
        , plural names "Did you mean" " one of these"
        , ": "
        , punctuate ", " names
        , "?"
        ]
    Shadowing l ns ->
      let names = foldMapSet (pure . f) ns in
      [ "You are redefining the "
      , levelMsg l
      , " ", plural names "variable" "s"
      , " ", punctuate ", " names
      , "."
      ]

instance Render ScopeError where
  render = pure . metaRender getName
  renderHtml = pure . metaRender identifier
-}

anExperiment :: MonadWriter (Seq Feedback) m => LineDoc -> [Name] -> Doc -> m ()
anExperiment str xs ls = tell $ Seq.singleton $ AnExperiment str xs ls

keep :: Options -> Feedback -> Bool
keep opts fdk
  = not (quiet opts)
  || (categorise fdk /= Comment
  && case fdk of { AStubbedOut{} -> False; _ -> True })

{-
fresh :: MonadState Int m => m Int
fresh = do
  n <- get
  let sn = n + 1
  put sn
  pure sn
-}

groupFeedback :: [Feedback] -> [Feedback]
groupFeedback (ACircuitDefined cs : ACircuitDefined es : rest) =
  groupFeedback (ACircuitDefined (cs ++ es) : rest)
groupFeedback (ATypeDefined cs : ATypeDefined es : rest) =
  groupFeedback (ATypeDefined (cs ++ es) : rest)
groupFeedback (fdk : rest) = fdk : groupFeedback rest
groupFeedback [] = []

instance Pretty [Feedback] where
  type PrettyDoc [Feedback] = Doc
  prettyPrec _ = intercalate (aLine "") . map pretty . groupFeedback

identifier :: Name -> LineDoc
identifier = isCode . pretty

tyIdentifier :: TyName -> LineDoc
tyIdentifier = isCode . pretty

instance Pretty Feedback where
  type PrettyDoc Feedback = Doc
  prettyPrec _ e = structure (StatusBlock $ categorise e) $ go e

    where

    go :: Feedback -> Doc
    go = \case
      AnImpossibleError str -> prettyBlock $$
        ["The IMPOSSIBLE has happened: ", str, "."]
      ACouldntFindCircuitDiagram nm -> aLine $$
        ["Could not find the diagram for the circuit ", identifier nm, "."]
      ACannotDisplayStub nm -> aLine $$
        ["Cannot display a diagram for the stubbed out circuit ", identifier nm, "."]

      AnExperiment d x ls ->
        aLine (fold [d, " ", punctuate ", " (identifier <$> x), ":"])
        <> nest 2 ls
{-
      ADotGraph xs x ls -> do
        n <- show <$> fresh
        pure $$$ let graphName = "GRAPH" ++ n in
          [ fold ["Displaying ", identifier x, extra, ":"]
          , br
          , Html.script ! type_ "module" $$$
              let dotName = "dot" <> toHtml n in
              let svgName = "svg" <> toHtml n in
              [ ""
              , "  import { Graphviz } from \"https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/index.js\";"
              , "  if (Graphviz) {"
              , "    const graphviz = await Graphviz.load();"
              , "    const " <> dotName <> " = " <> preEscapedString (show (unlines ls)) <> ";"
              , "    const " <> svgName <> " = graphviz.dot(" <> dotName <> ");"
              , "    document.getElementById(\"" <> toHtml graphName <> "\").innerHTML = " <> svgName <> ";"
              , "  }"
              , ""
              ]
          , div ! style "padding-left: 1em" $ div ! id (toValue graphName) $ ""
          ]
        where extra = case xs of
                [] -> ""
                _ -> fold [" (with ", punctuate ", " (map identifier xs), " unfolded)"]
-}
      AFoundHoles f ls ->
        aLine $$ ["Found holes in circuit ", identifier f, ":"]
        <> nest 2 (foldMap pretty ls)
      ALint ls -> ls

      ANoExecutable exe -> aLine $$
        [ "Could not find the ", isCode (pretty exe), " executable." ]
{-
      AnSVGGraph xs x ls -> pure $$
        [ fold ["Displaying ", identifier x, extra, ":"]
        , br
        , div ! style "padding-left: 1em" $ foldMap toHtml ls
        ]
        where extra = case xs of
                [] -> ""
                _ -> fold [" (with ", punctuate ", " (map identifier xs), " unfolded)"]
-}
      ASuccessfulUnitTest -> aLine "Success!"
      ARawCode str x ls ->
        aLine $$ [ str, " ", identifier x, ":" ]
        <> nest 2 (structure RawCodeBlock ls)
      ATruthTable x ls ->
        aLine $$ ["Truth table for ", identifier x, ":" ]
        <> nest 2 (structure PreBlock $ foldMap prettyBlock ls)
      AnUnreasonablyLargeExperiment lim size x -> aLine $$
        [ "Gave up on experimenting on ", identifier x
        , " due to its size (", pretty size
        , " but the limit is ", pretty lim,")."
        ]
{-
      ASyntaxError ls -> pure $$$ fmap toHtml ls
      AScopeError ls -> renderHtml ls
-}
      ACircuitDefined cs -> aLine $ punctuate " "
        [ plural cs "Circuit" "s"
        , oxfordList (map identifier cs)
        , be cs
        , "defined."
        ]
      ATypeDefined ts -> aLine $ punctuate " "
        [ plural ts "Type" "s"
        , oxfordList (map tyIdentifier ts)
        , be ts
        , "defined."
        ]
      AStubbedOut nm -> aLine $$
        [ "Circuit ", identifier nm, " has been stubbed out." ]
{-
      ATypeError ls -> pure $$ fmap toHtml ls
-}
      AnUnknownIdentifier x -> aLine $$
        [ "I don't know what ", identifier x, " is." ]
      AMissingImplementation x -> aLine $$
        [ "I don't have an implementation for ", identifier x, "." ]
      AnAmbiguousDefinition f zs ->
        aLine $$  [ "I don't know which of the following is your preferred ", identifier f, ":" ]
        <> nest 2 (foldMap (structure PreBlock . foldMap prettyBlock) zs)
      AnUndefinedCircuit f -> aLine $$
        [ "You haven't defined the circuit ", identifier f, " just now." ]
      AnUndeclaredCircuit f -> aLine $$
        [ "You haven't declared the circuit ", identifier f, " just now." ]
      AnUndefinedType x -> aLine $$
        [ "You haven't defined the type ", tyIdentifier x, " just now." ]
      AnInvalidTruthTableOutput f -> aLine $$
        [ "Invalid truth table output for ", identifier f, "." ]
      AnIllTypedInputs x iTys is ->
        aLine $$
          [ "Inputs for ", identifier x, " are typed "
          , isCode (csep $ map pretty iTys), "."
          ]
        <> aLine $$
          [ "That can't accept ", isCode (parens $$ map pretty is), "."
          ]
{-
      AnIllTypedMemory x mTys m0 -> do
        mTys <- traverse renderHtml mTys
        m0 <- traverse renderHtml m0
        pure $$
          [ "Memory for ", identifier x, " has type "
          , code (braces $ punctuate ", " mTys), "."
          , br
          , "That can't store ", braces $$ m0 ,"."
          ]
      AnIllTypedOutputs x oTys os -> do
        oTys <- traverse renderHtml oTys
        os <- traverse renderHtml os
        pure $$
          [ "Outputs for ", identifier x, " are typed "
          , code (punctuate ", " oTys), "."
          , br
          , "That can't accept "
          , code (punctuate ", " os), "."
          ]
      AWrongFinalMemory mo mo' -> do
        mo <- traverse renderHtml mo
        mo' <- traverse renderHtml mo'
        pure $$
          [ "Wrong final memory: expected "
          , code (braces $$ mo)
          , " but got "
          , code (braces $$ mo')
          , "." ]
      AWrongOutputSignals os os' -> do
        os <- traverse renderHtml os
        os' <- traverse renderHtml os'
        pure $$
          [ "Wrong output signals: expected "
          , code (parens $$ os)
          , " but got "
          , code (parens $$ os')
          , "." ]

      WhenUnitTesting x is os fdks -> do
        fdks <- traverse renderHtml fdks
        pure $$$
          [ fold [ "When unit testing ", identifier x
                 , toHtml (circuitConfig True is), " = "
                 , toHtml (circuitConfig False os), ":"]
          , br
          , div ! style "padding-left: 1em" $ punctuate (br <> "\n") fdks
          ]
      WhenDisplaying f fdks -> do
        fdks <- traverse renderHtml fdks
        pure $$$
          [ fold [ "When displaying ", identifier f, ":" ]
          , br
          , div ! style "padding-left: 1em" $ punctuate (br <> "\n") fdks
          ]

feedbackText :: [Feedback] -> [String]
feedbackText = render

feedbackHtml :: [Feedback] -> Html
feedbackHtml = (headerHtml <>) . flip evalState 0 . renderHtml

  where
    headerHtml :: Html
    headerHtml = (<> "\n") $ Html.style $ toHtml $ unlines
      [ ""
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
      , "    content: \"\\2705\";"
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
      , ""
      ]
-}
