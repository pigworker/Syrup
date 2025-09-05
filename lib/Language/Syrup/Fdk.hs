------------------------------------------------------------------------------
-----                                                                    -----
-----     Fdk: Feedback for Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Syrup.Fdk where

import Prelude hiding (div, id)
import qualified Prelude

import Control.Monad.State (MonadState, get, put, evalState)
import Control.Monad.Writer (MonadWriter, tell)

import Data.Foldable (fold)
import Data.List (intercalate, intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Void (Void, absurd)

import Language.Syrup.BigArray (Set, isEmptyArr, foldMapSet)
import Language.Syrup.Opt (Options(..), quiet)
import Language.Syrup.Syn.Base

import Text.Blaze.Html5
  (AttributeValue, Html, (!), br, code, div, pre, preEscapedString, toHtml, toValue)
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes
  (class_, id, style, type_)

($$) :: (Html -> a) -> [Html] -> a
f $$ x = f (fold x)

($$$) :: (Html -> a) -> [Html] -> a
f $$$ x = f $$ intersperse "\n" x

oxfordList :: (Monoid a, IsString a) => [a] -> a
oxfordList [] = ""
oxfordList [x] = x
oxfordList [x,y] = fold [x, " and ", y]
oxfordList xs = fold (go xs) where

  go = \case
    [] -> []
    [x] -> [x]
    [x,y] -> [x, ", and ", y]
    (x:xs) -> x : ", " : go xs

------------------------------------------------------------------------------
-- Feedback classes

type MonadRenderHtml m =
  ( MonadState Int m
  )

class Categorise t where
  categorise :: t -> FeedbackStatus

class Render t where
  render :: t -> [String]
  renderHtml :: MonadRenderHtml m => t -> m Html
  renderHtml = pure . toHtml . concat . render

instance Render Va where
  render = pure . show
  renderHtml = pure . toHtml . show

indent :: Int -> String -> String
indent n str = replicate n ' ' ++ str

plural :: Monoid s => [a] -> s -> s -> s
plural (_ : _ : _) str s = str <> s
plural _ str _ = str

between :: Monoid a => (a, a) -> a -> a
between (l,r) m = fold [l, m, r]

squares :: (Monoid a, IsString a) => a -> a
squares = between ("[", "]")

angles :: (Monoid a, IsString a) => a -> a
angles = between ("<", ">")

braces :: (Monoid a, IsString a) => a -> a
braces = between ("{", "}")

parens :: (Monoid a, IsString a) => a -> a
parens = between ("(", ")")

punctuate :: Monoid a => a -> [a] -> a
punctuate pun s = fold $ intersperse pun s

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


feedbackStatus :: FeedbackStatus -> String
feedbackStatus = \case
    Success -> ""
    Comment -> ""
    Warning -> "Warning"
    Error -> "Error"
    Internal -> "Internal error"

toCSSClass :: FeedbackStatus -> AttributeValue
toCSSClass st = toValue $ ("syrup-" ++) $ case st of
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

levelMsg :: IsString a => ScopeLevel -> a
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
  render = pure . metaRender Prelude.id
  renderHtml = pure . metaRender identifier

instance Render (Ty t Void) where
  render = \case
    Meta v -> absurd v
    TVar s _ -> [angles s]
    Bit{} -> pure "<Bit>"
    Cable ts -> [squares $ punctuate ", " $ foldMap render ts]

  renderHtml = pure . toHtml . concat . render

data Feedback
  -- internal errors
  = ACouldntFindCircuitDiagram String
  | AnImpossibleError String

  -- error
  | ACannotDisplayStub String
  | ANoExecutable String
  | AScopeError ScopeError
  | ASyntaxError [String]
  | ATypeError [String]
  | AnAmbiguousDefinition String [[String]]
  | AnInvalidTruthTableOutput String
  | AnUndeclaredCircuit String
  | AnUndefinedCircuit String
  | AnUndefinedType String
  | AnUnknownIdentifier String
  | AnIllTypedInputs String [Ty Unit Void] [Va]
  | AnIllTypedMemory String [Ty Unit Void] [Va]
  | AnIllTypedOutputs String [Ty Ti Void] [Va]
  | AWrongFinalMemory [Va] [Va]
  | AWrongOutputSignals [Va] [Va]

  -- warnings
  | AFoundHoles String [String]
  | ALint [String]
  | AMissingImplementation String
  | AStubbedOut String
  | AnUnreasonablyLargeExperiment Int Int String

  -- comments
  | ACircuitDefined [String] -- non empty list
  | ATypeDefined [String] -- non empty list

  -- successes
  | ADotGraph [String] String [String]
  | ARawCode String String [String]
  | ATruthTable String [String]
  | AnExperiment String [String] [String]
  | AnSVGGraph [String] String [String]
  | ASuccessfulUnitTest

  -- contextual
  | WhenDisplaying String [Feedback]
  | WhenUnitTesting String CircuitConfig CircuitConfig [Feedback]

instance Categorise Feedback where
  categorise = \case
    -- internal errors
    AnImpossibleError{} -> Internal
    ACouldntFindCircuitDiagram{} -> Internal

    -- errors
    ACannotDisplayStub{} -> Error
    ANoExecutable{} -> Error
    AScopeError{} -> Error
    ASyntaxError{} -> Error
    ATypeError{} -> Error
    AnAmbiguousDefinition{} -> Error
    AnInvalidTruthTableOutput{} -> Error
    AnUndeclaredCircuit{} -> Error
    AnUndefinedCircuit{} -> Error
    AnUndefinedType{} -> Error
    AnUnknownIdentifier{} -> Error
    AnIllTypedInputs{} -> Error
    AnIllTypedMemory{} -> Error
    AnIllTypedOutputs{} -> Error
    AWrongFinalMemory{} -> Error
    AWrongOutputSignals{} -> Error

    -- warnings
    AFoundHoles{} -> Warning
    ALint{} -> Warning
    AMissingImplementation{} -> Warning
    AStubbedOut{} -> Warning
    AnUnreasonablyLargeExperiment{} -> Warning

    -- comments
    ACircuitDefined{} -> Comment
    ATypeDefined{} -> Comment

    -- successes
    ADotGraph{} -> Success
    ARawCode{} -> Success
    ATruthTable{} -> Success
    AnExperiment{} -> Success
    AnSVGGraph{} -> Success
    ASuccessfulUnitTest{} -> Success

    -- contextual
    WhenDisplaying _ fdks -> foldMap categorise fdks
    WhenUnitTesting _ _ _ fdks -> foldMap categorise fdks

anExperiment :: MonadWriter (Seq Feedback) m => String -> [String] -> [String] -> m ()
anExperiment str xs ls = tell $ Seq.singleton $ AnExperiment str xs ls

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

identifier :: String -> Html
identifier = code . toHtml

groupFeedback :: [Feedback] -> [Feedback]
groupFeedback (ACircuitDefined cs : ACircuitDefined es : rest) =
  groupFeedback (ACircuitDefined (cs ++ es) : rest)
groupFeedback (ATypeDefined cs : ATypeDefined es : rest) =
  groupFeedback (ATypeDefined (cs ++ es) : rest)
groupFeedback (fdk : rest) = fdk : groupFeedback rest
groupFeedback [] = []

instance Render [Feedback] where
  render = intercalate [""] . map render . groupFeedback
  renderHtml = fmap (punctuate (br <> "\n")) . traverse renderHtml . groupFeedback

instance Render Feedback where
  render e =
    let preamb = feedbackStatus $ categorise e in
    prepend (plural preamb preamb ": ") (go e) where

    prepend :: Semigroup a => a -> [a] -> [a]
    prepend x []       = [x]
    prepend x (y : xs) = (x <> y : xs)

    go = \case
      AnImpossibleError str -> ["The IMPOSSIBLE has happened: " ++ str ++ "."]
      ACouldntFindCircuitDiagram nm -> ["Could not find the diagram for the circuit " ++ nm ++ "."]
      ACannotDisplayStub nm ->  ["Cannot display a diagram for the stubbed out circuit " ++ nm ++ "."]
      ACircuitDefined cs -> pure $ unwords
        [ plural cs "Circuit" "s"
        , oxfordList cs
        , case cs of { (_:_:_) -> "are"; _ -> "is" }
        , "defined."
        ]
      ATypeDefined ts -> pure $ unwords
        [ plural ts "Type" "s"
        , oxfordList (map angles ts)
        , case ts of { (_:_:_) -> "are"; _ -> "is" }
        , "defined."
        ]
      ADotGraph xs x ls -> ("Displaying " ++ x ++ extra ++ ":") : ls
        where extra = case xs of
                [] -> ""
                _ -> concat [" (with ", intercalate ", " xs, " unfolded)"]
      AFoundHoles f ls -> ("Found holes in circuit " ++ f ++ ":") : ls
      ALint ls -> ls
      AMissingImplementation x -> ["I don't have an implementation for " ++ x ++ "."]
      ANoExecutable exe -> ["Could not find the " ++ exe ++ " executable :("]
      ARawCode str x ls -> (str ++ " " ++ x ++ ":") : ls
      AScopeError ls -> render ls
      AStubbedOut nm -> ["Circuit " ++ nm ++ " has been stubbed out."]
      AnUnreasonablyLargeExperiment lim size x ->
        ["Gave up on experimenting on " ++ x ++ " due to its size (" ++ show size
           ++ " but the limit is " ++ show lim ++ ")."]
      ASyntaxError ls -> ls
      ATruthTable x ls -> ("Truth table for " ++ x ++ ":") : ls
      ATypeError ls -> ls
      AnAmbiguousDefinition f zs ->
        ["I don't know which of the following is your preferred " ++ f ++ ":"]
        ++ intercalate [""] zs
      AnExperiment str xs ls -> (str ++ " " ++ intercalate ", " xs ++ ":") : ls
      AnSVGGraph xs x ls ->  ("Displaying " ++ x ++ extra ++ ":") : ls
        where extra = case xs of
                [] -> ""
                _ -> concat [" (with ", intercalate ", " xs, " unfolded)"]
      ASuccessfulUnitTest -> [ "Success!" ]
      AnUndeclaredCircuit f -> ["You haven't declared the circuit " ++ f ++ " just now."]
      AnUndefinedCircuit f -> ["You haven't defined the circuit " ++ f ++ " just now."]
      AnUndefinedType x -> ["You haven't defined the type alias " ++ x ++ " just now."]
      AnUnknownIdentifier x -> ["I don't know what " ++ x ++ " is."]
      AnInvalidTruthTableOutput f -> ["Invalid truth table output for " ++ f ++ "."]
      AnIllTypedInputs x iTys is ->
        [ concat ["Inputs for ", x, " are typed (", intercalate ", " (foldMap render iTys), ")."]
        , concat ["That can't accept (", foldMap show is, ")."]
        ]
      AnIllTypedMemory x mTys m0 ->
        [ concat ["Memory for ", x, " has type {", intercalate ", " (foldMap render mTys), "}."]
        , concat ["That can't store {", foldMap show m0, "}."]
        ]
      AnIllTypedOutputs x oTys os ->
        [ concat ["Outputs for ", x, " are typed ", intercalate ", " (foldMap render oTys), "."]
        , concat ["That can't accept ", foldMap show os, "."]
        ]
      AWrongFinalMemory mo mo' -> pure $ concat
        [ "Wrong final memory: expected {", foldMap show mo, "} but got {", foldMap show mo', "}." ]
      AWrongOutputSignals os os' -> pure $ concat
        [ "Wrong output signals: expected ", foldMap show os, " but got ", foldMap show os', "." ]


      WhenUnitTesting x is os fdks ->
        concat [ "When unit testing ", x, circuitConfig True is, " = ", circuitConfig False os, ":" ]
        : concatMap (map (indent 2) . render) fdks
      WhenDisplaying f fdks -> ("When displaying " ++ f ++ ":") : concatMap (map (indent 2) . render) fdks


  renderHtml e = do
    let cat = categorise e
    msg <- goHtml e
    pure $ div ! class_ (toCSSClass cat) $ msg


    where

    goHtml :: MonadRenderHtml m => Feedback -> m Html
    goHtml = \case
      AnImpossibleError str -> pure $$ ["The IMPOSSIBLE has happened: ", toHtml str, "."]
      ACouldntFindCircuitDiagram nm -> pure $$
        ["Could not find the diagram for the circuit ", identifier nm, "."]
      ACannotDisplayStub nm -> pure $$
        ["Cannot display a diagram for the stubbed out circuit ", identifier nm, "."]

      AnExperiment str x ls -> pure $$$
        [ fold [toHtml str, " ", punctuate ", " (identifier <$> x), ":"]
        , br
        , foldMap toHtml ls
        ]
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

      AFoundHoles f ls -> pure $$$
        [ fold ["Found holes in circuit ", identifier f, ":"]
        , br
        , div ! style "padding-left: 1em" $ foldMap toHtml ls
        ]
      ALint ls -> pure $$ fmap toHtml ls
      ANoExecutable exe -> pure $$
        [ "Could not find the ", identifier exe, " executable :(" ]
      AnSVGGraph xs x ls -> pure $$
        [ fold ["Displaying ", identifier x, extra, ":"]
        , br
        , div ! style "padding-left: 1em" $ foldMap toHtml ls
        ]
        where extra = case xs of
                [] -> ""
                _ -> fold [" (with ", punctuate ", " (map identifier xs), " unfolded)"]
      ASuccessfulUnitTest -> pure "Success!"
      ARawCode str x ls -> pure $$$
        [ fold [ toHtml str, " ", identifier x, ":" ]
        , br
        , div ! style "padding-left: 1em" $ div ! class_ "syrup-code" $ (toHtml $ unlines ls)
        ]
      ATruthTable x ls -> pure $$$
        [ fold ["Truth table for ", identifier x, ":"]
        , br
        , div ! style "padding-left: 1em" $ pre (toHtml $ unlines ls)
        ]
      AnUnreasonablyLargeExperiment lim size x -> pure $$
        [ "Gave up on experimenting on ", identifier x
        , " due to its size (", toHtml (show size)
        , " but the limit is ", toHtml (show lim),")."
        ]
      ASyntaxError ls -> pure $$ fmap toHtml ls
      AScopeError ls -> renderHtml ls
      ACircuitDefined cs -> pure $ punctuate " "
        [ plural cs "Circuit" "s"
        , oxfordList (map (code . toHtml) cs)
        , case cs of { (_:_:_) -> "are"; _ -> "is" }
        , "defined."
        ]
      ATypeDefined ts -> pure $ punctuate " "
        [ plural ts "Type" "s"
        , oxfordList (map (code . angles . toHtml) ts)
        , case ts of { (_:_:_) -> "are"; _ -> "is" }
        , "defined."
        ]
      AStubbedOut nm -> pure $$
        [ "Circuit ", identifier nm, " has been stubbed out." ]
      ATypeError ls -> pure $$ fmap toHtml ls
      AnUnknownIdentifier x -> pure $$
        [ "I don't know what ", identifier x, " is." ]
      AMissingImplementation x -> pure $$
        [ "I don't have an implementation for ", identifier x, "." ]
      AnAmbiguousDefinition f zs -> pure $$$
        [ fold [ "I don't know which of the following is your preferred ", identifier f, ":" ]
        , br
        , div ! style "padding-left: 1em" $ punctuate br (map (pre . toHtml . punctuate "\n") zs)
        ]
      AnUndefinedCircuit f -> pure $$
        [ "You haven't defined the circuit ", identifier f, " just now." ]
      AnUndeclaredCircuit f -> pure $$
        [ "You haven't declared the circuit ", identifier f, " just now." ]
      AnUndefinedType x -> pure $$
        [ "You haven't defined the type ", identifier x, " just now." ]
      AnInvalidTruthTableOutput f -> pure $$
        [ "Invalid truth table output for ", identifier f, "." ]
      AnIllTypedInputs x iTys is -> do
        iTys <- traverse renderHtml iTys
        is <- traverse renderHtml is
        pure $$
          [ "Inputs for ", identifier x, " are typed "
          , code (parens $ punctuate ", " iTys), "."
          , br
          , "That can't accept "
          , code (parens $$ is), "."
          ]
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
