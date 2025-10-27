------------------------------------------------------------------------------
-----                                                                    -----
-----     Doc: Documents for Syrup                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.Syrup.Doc
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
  ) where

import Prelude hiding (unlines, unwords)

import Data.Foldable (fold)
import Data.List (intersperse)

import qualified Text.PrettyPrint.Annotated as Doc
import Text.PrettyPrint.Annotated
  (annotate, braces, brackets, hcat, hsep, nest, parens, text, vcat)

------------------------------------------------------------------------
-- Doc type and basic combinators

data SyrupAnnotations
  = Keyword
  | Type
  | Function

type Doc = Doc.Doc SyrupAnnotations

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
