------------------------------------------------------------------------------
-----                                                                    -----
-----     Doc: Documents for Syrup                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Syrup.Doc
  -- Doc stuff
  ( LineDoc
  , TABLE(..), ROW(..), CELL(..)
  , Doc
  , Render(..)
  , AnnHighlight(..)
  , AnnStructure(..)
  , FeedbackStatus(..)
  , isErroring
  , aGraph
  , aLine
  , aString
  , aTable
  , highlight
  , isCode
  , structure
  , nest
  , between
  , braces
  , brackets
  , csep
  , list
  , parens
  , parensIf
  , punctuate
  , set
  , tuple
  , unwords
  -- Pretty stuff
  , Pretty(..)
  , prettyBlock
  , PrecedenceLevel(..)
  , AList(..)
  , ATuple(..)
  , ASet(..)
  ) where

import Prelude hiding (unwords)

import Data.Foldable (fold)
import Data.Kind (Type)
import Data.List (intercalate, intersperse)
import Data.String (IsString, fromString)
import Data.Void (Void, absurd)

import Text.Blaze.Html5 (Html, AttributeValue, (!), toHtml, toValue)
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes (class_, style, type_)
import qualified Text.Blaze.Html5.Attributes as Attr

import Language.Syrup.Syn.Base
import Language.Syrup.Utils (padRightWith, plural, ($$))
import Utilities.Monad.Fresh (MonadFresh, fresh, renderFresh)

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

------------------------------------------------------------------------
-- Doc type and renderers

data AnnHighlight
  = AFunction
  | AVariable
  | AKeyword
  | AType

data AnnLine
  = IsCode
  | HasStyle AnnHighlight

data AnnStructure
  = NestBlock Int
  | PreBlock
  | RawCodeBlock
  | StatusBlock FeedbackStatus
  | ClassBlock String
  | DetailsBlock LineDoc

data LineDoc
  = AString String
  | AnAnnot AnnLine LineDoc
  | AConcat [LineDoc]

aString :: String -> LineDoc
aString = AString

highlight :: AnnHighlight -> LineDoc -> LineDoc
highlight = AnAnnot . HasStyle

isCode :: LineDoc -> LineDoc
isCode = AnAnnot IsCode

structure :: AnnStructure -> Doc -> Doc
structure ann d = [ABlock (Just ann) d]

nest :: Int -> Doc -> Doc
nest i d | i <= 0 || null d = d
nest i d = structure (NestBlock i) d

aLine :: LineDoc -> Doc
aLine = pure . ALine

aGraph :: [String] -> Doc
aGraph = pure . AGraph

aTable :: TABLE LineDoc -> Doc
aTable = pure . ATable

instance Semigroup LineDoc where
  AString "" <> d = d
  d <> AString "" = d
  d@(AString{}) <> AConcat ds = AConcat (d : ds)
  d1 <> d2 = AConcat [d1, d2]

instance Monoid LineDoc where
  mempty = AString ""

data BlockDoc
  = ALine LineDoc
  | ABlock (Maybe AnnStructure) Doc
  | AGraph [String]
  | ATable (TABLE LineDoc)

data TABLE a = TABLE
-- we assume that all of the lists have the same length
  { tHead :: Maybe [a] -- optional thead
  , tBody :: [ROW a] -- rows
  } deriving (Functor, Foldable, Traversable)

data CELL a
  = ATH a
  | ATD a
  deriving (Functor, Foldable, Traversable)

data ROW a
  = SimpleRow [CELL a]
  | MultiRow [CELL a] [[CELL a]]
  deriving (Functor, Foldable, Traversable)

isMultiRow :: ROW a -> Bool
isMultiRow (SimpleRow _) = False
isMultiRow (MultiRow _ _) = True

instance Semigroup BlockDoc where
  d <> ABlock Nothing [] = d
  ABlock Nothing d1 <> ABlock Nothing d2 = ABlock Nothing (d1 ++ d2)
  d1 <> d2 = ABlock Nothing [d1, d2]

type Doc = [BlockDoc]

instance IsString LineDoc where
  fromString = AString

class Render d where
  renderToString :: d -> [String]
  renderToHtml :: MonadFresh s m => d -> m Html

instance Render LineDoc where

  renderToString = pure . go where

    go :: LineDoc -> String
    go (AString str) = str
    go (AnAnnot IsCode d) = "`" ++ go d ++ "`"
    go (AnAnnot (HasStyle _) d) = go d
    go (AConcat ds) = foldMap go ds


  renderToHtml = pure . go where

    go :: LineDoc -> Html
    go (AConcat ds) = foldMap go ds
    go (AString str) = toHtml str
    go (AnAnnot ann d) = applyHighlight ann (go d)

    applyHighlight :: AnnLine -> Html -> Html
    applyHighlight IsCode = Html.code
    applyHighlight (HasStyle sty) = Html.span ! class_ (asAttribute sty)

    asAttribute :: AnnHighlight -> AttributeValue
    asAttribute AFunction = "syrup-function"
    asAttribute AVariable = "syrup-variable"
    asAttribute AKeyword = "syrup-keyword"
    asAttribute AType = "syrup-type"


instance Render Doc where

  renderToString = foldMap renderBlock where

    renderBlock :: BlockDoc -> [String]
    renderBlock (ALine d) = [concat $ renderToString d]
    renderBlock (ABlock ann ds)
      = maybe id applyStructure ann
      $ foldMap renderBlock ds
    renderBlock (AGraph ls) = ls
    renderBlock (ATable tb) =
      let TABLE mhd rs = fmap (concat . renderToString) tb in
      let ws = sizes mhd rs in
      maybe id (\ hd -> (renderHead ws hd ++)) mhd
      $ concat
      $ (if any isMultiRow rs then intersperse ([separator ws]) else id)
      $ map (renderRow ws) rs

      where

        sizes :: Maybe [String] -> [ROW String] -> [Int]
        sizes mhd rs = foldr (zipWith max)
          (maybe (repeat 0) (map length) mhd)
          (map sizeROW rs)

        sizeROW :: ROW String -> [Int]
        sizeROW (SimpleRow tdhs) = map (sizeCELL True) tdhs
        sizeROW (MultiRow hds tdhs) =
          map (sizeCELL True) hds ++
          foldr (zipWith max) (repeat 0) (map (map (sizeCELL True)) tdhs)

        -- The Bool tells us whether the TH will have a colon attached to the end of it
        sizeCELL :: Bool -> CELL String -> Int
        sizeCELL b (ATH s) = if b then 1 + length s else length s
        sizeCELL _ (ATD s) = length s

        smartPadWith :: Char -> Int -> String -> String
        smartPadWith c n str
          | n <= 0 = ""
          | otherwise = c : padRightWith c (1 + n - length str) str

        separator :: [Int] -> String
        separator ws = "|" ++ intercalate "+" (map (flip (smartPadWith '-') "") ws) ++ "|" where


        renderHead :: [Int] -> [String] -> [String]
        renderHead ws ths =
          [ "|" ++ intercalate "|" (zipWith (smartPadWith ' ') ws ths) ++ "|"
          , separator ws
          ]

        renderCell :: CELL String -> String
        renderCell (ATH th) = th ++ ":"
        renderCell (ATD td) = td

        renderRow :: [Int] -> ROW String -> [String]
        renderRow ws (SimpleRow thds) = pure $ concat
          [ "|"
          , intercalate "|" (zipWith (\ w -> smartPadWith ' ' w . renderCell) ws thds)
          , "|"
          ]
        renderRow ws (MultiRow shs tdhss) = concatMap (renderRow ws . SimpleRow)
          $ zipWith (++) (shs : repeat (ATD "" <$ shs)) tdhss


    applyStructure :: AnnStructure -> [String] -> [String]
    applyStructure (NestBlock i) ls
      | i > 0 = map (replicate i ' ' ++) ls
      | otherwise = ls
    applyStructure (StatusBlock cat) [] = []
    applyStructure (StatusBlock cat) (l : ls) =
      let status = feedbackStatus cat in
      (plural status status ": " <> l) : ls
    applyStructure PreBlock ls = ls
    applyStructure RawCodeBlock ls = ls
    applyStructure (ClassBlock cl) ls = ls
    applyStructure (DetailsBlock s) ls =
      concat (renderToString s) : ls


  renderToHtml = go False where

    -- The Bool is True if we are in pre mode, in which case we do:
    -- 1. newlines as "\n" alone rather than (br <> "\n")

    newline :: Bool -> Html
    newline b = (if b then id else (("\n" <> Html.br) <>)) "\n"

    go :: MonadFresh s m => Bool -> Doc -> m Html
    go b = fmap (fold . intersperse (newline b))
         . renderBlocks b

    renderBlocks :: MonadFresh s m => Bool -> Doc -> m [Html]
    renderBlocks b = fmap fold . traverse (renderBlock b)

    renderBlock :: MonadFresh s m => Bool -> BlockDoc -> m [Html]
    renderBlock b (ALine d) = pure <$> renderToHtml d
    renderBlock b (ABlock Nothing ds) = renderBlocks b ds
    renderBlock b (ABlock (Just ann) ds) = case ann of
      NestBlock i ->
        if i <= 0 then renderBlocks b ds else do
          html <- go b ds
          pure $ pure $ Html.div
            ! style (toValue $ "padding-left: " ++ show i ++ "ch")
            $ html
      PreBlock -> (pure . Html.pre) <$> go True ds
      RawCodeBlock -> do
        html <- go True ds
        pure $ [Html.div ! class_ "syrup-code" $ html]
      StatusBlock cat -> do
        html <- go False ds
        pure [Html.div ! class_ (toCSSClass cat) $ html]
      ClassBlock cl -> do
        html <- go False ds
        pure $ [Html.div ! class_ (toValue cl) $ html]
      DetailsBlock s -> do
        html <- go b ds
        s <- renderToHtml s
        pure $ pure $ Html.details $ do
          Html.summary s
          html
    renderBlock _ (AGraph ls) = do
        n <- renderFresh <$> fresh
        pure $ let graphName = "GRAPH" ++ n in
          [ Html.script ! type_ "module" $ fold $ intersperse "\n" $
              let dotName = "dot" <> toHtml n in
              let svgName = "svg" <> toHtml n in
              [ ""
              , "  import { Graphviz } from \"https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/index.js\";"
              , "  if (Graphviz) {"
              , "    const graphviz = await Graphviz.load();"
              , "    const " <> dotName <> " = " <> Html.preEscapedString (show (unlines ls)) <> ";"
              , "    const " <> svgName <> " = graphviz.dot(" <> dotName <> ");"
              , "    document.getElementById(\"" <> toHtml graphName <> "\").innerHTML = " <> svgName <> ";"
              , "  }"
              , ""
              ]
          , Html.div ! Attr.id (toValue graphName) $ ""
          ]
    renderBlock _ (ATable tb) = do
      TABLE mhd rs <- traverse renderToHtml tb
      pure $ pure $ Html.table $ do
        maybe "" mkHead mhd
        Html.tbody $$ intersperse "\n" (fmap mkRow rs)

      where
        mkHead :: [Html] -> Html
        mkHead ths = (Html.thead . Html.tr) $$ fmap Html.th ths

        mkCell :: Maybe Html.Attribute -> CELL Html -> Html
        mkCell ma (ATH th) = foldl (!) Html.th ma th
        mkCell ma (ATD td) = foldl (!) Html.td ma td

        mkRow :: ROW Html -> Html
        mkRow (SimpleRow tdhs) = Html.tr $$ fmap (mkCell Nothing) tdhs
        mkRow (MultiRow shs tdhss) = id $$
          let numrows = length tdhss in
          zipWith (\ l r -> Html.tr $$ (l ++ r))
            (map (mkCell (Just $ Attr.rowspan (toValue $ show numrows))) shs : repeat [])
            (fmap (fmap (mkCell Nothing)) tdhss)

------------------------------------------------------------------------
-- Basic combinators

between :: Monoid d => d -> d -> (d -> d)
between left right middle = fold [left, middle, right]

parens :: LineDoc -> LineDoc
parens = between "(" ")"

brackets :: LineDoc -> LineDoc
brackets = between "[" "]"

braces :: LineDoc -> LineDoc
braces = between "{" "}"

parensIf :: Bool -> LineDoc -> LineDoc
parensIf True  = parens
parensIf False = id

punctuate :: LineDoc -> [LineDoc] -> LineDoc
punctuate _ [] = ""
punctuate sep ds = AConcat (intersperse sep ds)

unwords :: [LineDoc] -> LineDoc
unwords = punctuate " "

csep :: [LineDoc] -> LineDoc
csep = punctuate ", "

list :: [LineDoc] -> LineDoc
list =  brackets . csep

tuple :: [LineDoc] -> LineDoc
tuple = parens . csep

set :: [LineDoc] -> LineDoc
set = braces . csep

------------------------------------------------------------------------
-- Pretty infrastructure

data PrecedenceLevel
  = OrClause
  | AndClause
  | NegatedClause
  deriving (Eq, Ord, Enum, Bounded)

newtype AList a = AList [a]
newtype ATuple a = ATuple [a]
newtype ASet a = ASet [a]

class Pretty t where
  type PrettyDoc t :: Type
  pretty :: t -> PrettyDoc t
  pretty = prettyPrec minBound

  prettyPrec :: PrecedenceLevel -> t -> PrettyDoc t

instance Pretty String where
  type PrettyDoc String = LineDoc
  prettyPrec _ = fromString

instance Pretty Name where
  type PrettyDoc Name = LineDoc
  prettyPrec _ = highlight AFunction . pretty . getName

instance Pretty TyName where
  type PrettyDoc TyName = LineDoc
  prettyPrec _ = highlight AType . pretty . between "<" ">" . getTyName

instance Pretty Integer where
  type PrettyDoc Integer = LineDoc
  prettyPrec _ = pretty . show

instance Pretty Int where
  type PrettyDoc Int = LineDoc
  prettyPrec _ = pretty . show

instance Pretty Void where
  type PrettyDoc Void = LineDoc
  prettyPrec _ = absurd

instance Pretty () where
  type PrettyDoc () = LineDoc
  prettyPrec _ _ = "()"

instance Pretty Unit where
  type PrettyDoc Unit = LineDoc
  prettyPrec _ _ = ""

instance (Pretty a, PrettyDoc a ~ LineDoc) => Pretty (AList a) where
  type PrettyDoc (AList a) = LineDoc
  prettyPrec _ (AList xs) = list $ map pretty xs

instance (Pretty a, PrettyDoc a ~ LineDoc) => Pretty (ATuple a) where
  type PrettyDoc (ATuple a) = LineDoc
  prettyPrec _ (ATuple xs) = tuple $ map pretty xs

instance (Pretty a, PrettyDoc a ~ LineDoc) => Pretty (ASet a) where
  type PrettyDoc (ASet a) = LineDoc
  prettyPrec _ (ASet xs) = set $ map pretty xs

instance Pretty LineDoc where
  type PrettyDoc LineDoc = Doc
  prettyPrec _ l = [ALine l]

prettyBlock :: (Pretty a, PrettyDoc a ~ LineDoc) => a -> Doc
prettyBlock = aLine . pretty
