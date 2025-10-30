------------------------------------------------------------------------------
-----                                                                    -----
-----     Doc: Documents for Syrup                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.Syrup.Doc
  -- Doc stuff
  ( LineDoc
  , Doc
  , renderToString
  , renderToHtml
  , AnnHighlight(..)
  , AnnStructure(..)
  , highlight
  , structure
  , between
  , braces
  , brackets
  , csep
  , list
  , nest
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
import Data.List (intersperse)
import Data.String (IsString, fromString)
import Data.Void (Void, absurd)

import Text.Blaze.Html5 (Html, AttributeValue, (!), toHtml, toValue)
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes (class_, style)

import Language.Syrup.Syn.Base

------------------------------------------------------------------------
-- Doc type and renderers

data AnnHighlight
  = AFunction
  | AKeyword
  | AType

data AnnStructure
  = NestBlock Int
  | CodeBlock
  | PreBlock
  | SVGBlock

data LineDoc
  = AString (Maybe AnnHighlight) String
  | AConcat [LineDoc]

highlight :: AnnHighlight -> String -> LineDoc
highlight ann str = AString (Just ann) str

structure :: AnnStructure -> Doc -> Doc
structure ann d = [ABlock (Just ann) d]

instance Semigroup LineDoc where
  AString _ "" <> d = d
  d <> AString _ "" = d
  d@(AString{}) <> AConcat ds = AConcat (d : ds)
  d1 <> d2 = AConcat [d1, d2]

instance Monoid LineDoc where
  mempty = AString Nothing ""

data BlockDoc
  = ALine LineDoc
  | ABlock (Maybe AnnStructure) Doc

instance Semigroup BlockDoc where
  ABlock Nothing [] <> d = d
  d <> ABlock Nothing [] = d
  ABlock Nothing d1 <> ABlock Nothing d2 = ABlock Nothing (d1 ++ d2)
  d1 <> d2 = ABlock Nothing [d1, d2]

type Doc = [BlockDoc]

instance IsString LineDoc where
  fromString = AString Nothing

renderToString :: Doc -> String
renderToString = unlines . render where

  render :: Doc -> [String]
  render = foldMap renderBlock

  renderLine :: LineDoc -> String
  renderLine (AString _ str) = str
  renderLine (AConcat ds) = foldMap renderLine ds

  renderBlock :: BlockDoc -> [String]
  renderBlock (ALine d) = [renderLine d]
  renderBlock (ABlock ann ds) = maybe id applyStructure ann $ render ds

  applyStructure :: AnnStructure -> [String] -> [String]
  applyStructure (NestBlock i) | i > 0 = map (replicate i ' ' ++)
  applyStructure _ = id

renderToHtml :: Doc -> Html
renderToHtml = vcat . render where

  vcat :: [Html] -> Html
  vcat = fold . intersperse (Html.br <> "\n")

  render :: Doc -> [Html]
  render = foldMap renderBlock

  renderLine :: LineDoc -> Html
  renderLine (AString ann str) = maybe id applyHighlight ann $ toHtml str
  renderLine (AConcat ds) = foldMap renderLine ds

  applyHighlight :: AnnHighlight -> Html -> Html
  applyHighlight ann html = Html.span ! class_ (asAttribute ann) $ html

  asAttribute :: AnnHighlight -> AttributeValue
  asAttribute AFunction = "syrup-function"
  asAttribute AKeyword = "syrup-keyword"
  asAttribute AType = "syrup-type"

  renderBlock :: BlockDoc -> [Html]
  renderBlock (ALine d) = [renderLine d]
  renderBlock (ABlock Nothing ds) = render ds
  renderBlock (ABlock (Just ann) ds) = case ann of
    NestBlock i ->
      if i < 0 then render ds else pure $
      Html.div
        ! style (toValue $ "padding-left: " ++ show i ++ "ch")
        $ renderToHtml ds
    CodeBlock -> pure $ Html.code $ renderToHtml ds
    PreBlock -> pure $ Html.code $ renderToHtml ds
    SVGBlock -> pure $ renderToHtml ds

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

nest :: Int -> Doc -> Doc
nest 0 d = d
nest i [] = []
nest i d = [ABlock (Just (NestBlock i)) d]

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

class Pretty d t where
  pretty :: t -> d
  pretty = prettyPrec minBound

  prettyPrec :: PrecedenceLevel -> t -> d

instance Pretty LineDoc String where
  prettyPrec _ = fromString

instance Pretty LineDoc Name where
  prettyPrec _ = highlight AFunction . getName

instance Pretty LineDoc TyName where
  prettyPrec _ = highlight AType . between "<" ">" . getTyName

instance Pretty LineDoc Integer where
  prettyPrec _ = pretty . show

instance Pretty LineDoc Void where
  prettyPrec _ = absurd

instance Pretty LineDoc () where
  prettyPrec _ _ = "()"

instance Pretty LineDoc Unit where
  prettyPrec _ _ = ""

instance Pretty LineDoc a => Pretty LineDoc (AList a) where
  prettyPrec _ (AList xs) = list $ map pretty xs

instance Pretty LineDoc a => Pretty LineDoc (ATuple a) where
  prettyPrec _ (ATuple xs) = tuple $ map pretty xs

instance Pretty LineDoc a => Pretty LineDoc (ASet a) where
  prettyPrec _ (ASet xs) = set $ map pretty xs

instance Pretty Doc LineDoc where
  prettyPrec _ l = [ALine l]

prettyBlock :: Pretty LineDoc a => a -> Doc
prettyBlock a = [ALine (pretty a)]
