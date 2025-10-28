------------------------------------------------------------------------------
-----                                                                    -----
-----     Doc: Documents for Syrup                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.Syrup.Doc
  -- Doc stuff
  ( Doc
  , SyrupAnnotations(..)
  , annotate
  , between
  , braces
  , brackets
  , csep
  , hcat
  , hsep
  , list
  , nest
  , parens
  , parensIf
  , punctuate
  , questionMark
  , set
  , text
  , tuple
  , unlines
  , unwords
  , vcat
  -- Pretty stuff
  , Pretty(..)
  , PrecedenceLevel(..)
  , AList(..)
  , ATuple(..)
  , ASet(..)
  ) where

import Prelude hiding (unlines, unwords)

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Void (Void, absurd)

import qualified Text.PrettyPrint.Annotated as Doc
import Text.PrettyPrint.Annotated
  (annotate, braces, brackets, hcat, hsep, nest, parens, text, vcat)

import Language.Syrup.Syn.Base

------------------------------------------------------------------------
-- Doc type and renderers

data SyrupAnnotations
  = AnnKeyword
  | AnnType
  | AnnFunction
  | AnnCodeBlock

type Doc = Doc.Doc SyrupAnnotations

------------------------------------------------------------------------
-- Basic combinators

between :: Doc -> Doc -> (Doc -> Doc)
between left right middle = fold [left, middle, right]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

unwords :: [Doc] -> Doc
unwords = hsep -- punctuate " "

unlines :: [Doc] -> Doc
unlines = vcat -- sepBy "\n"

csep :: [Doc] -> Doc
csep = punctuate ", "

list :: [Doc] -> Doc
list =  brackets . csep

punctuate :: Monoid a => a -> [a] -> a
punctuate pun s = fold $ intersperse pun s

tuple :: [Doc] -> Doc
tuple = parens . csep

set :: [Doc] -> Doc
set = braces . csep

questionMark :: Doc
questionMark = "?"

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
  pretty :: t -> Doc
  pretty = prettyPrec minBound

  prettyPrec :: PrecedenceLevel -> t -> Doc

instance Pretty String where
  prettyPrec _ = text

instance Pretty Name where
  prettyPrec _ = pretty . getName

instance Pretty TyName where
  prettyPrec _ = pretty . getTyName

instance Pretty Integer where
  prettyPrec _ = pretty . show

instance Pretty Void where
  prettyPrec _ = absurd

instance Pretty () where
  prettyPrec _ _ = "()"

instance Pretty Unit where
  prettyPrec _ _ = ""

instance Pretty a => Pretty (AList a) where
  prettyPrec _ (AList xs) = list $ map pretty xs

instance Pretty a => Pretty (ATuple a) where
  prettyPrec _ (ATuple xs) = tuple $ map pretty xs

instance Pretty a => Pretty (ASet a) where
  prettyPrec _ (ASet xs) = set $ map pretty xs
