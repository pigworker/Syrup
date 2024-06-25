------------------------------------------------------------------------------
-----                                                                    -----
-----     Pty: Pretty Printing library                                   -----
-----     This is based on Bernardy's functional pearl                   -----
-----     "A pretty but not greedy printer"                              -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Syrup.Pty
       ( Doc
       , render
       , empty
       , char
       , text
       , spaces
       , fail
       , parens
       , indent
       , flush
       , (<+>)
       , ($$)
       , (<|>)
       , foldDoc
       , hsep
       , vcat
       , sep
       , commaSep
       , semi, colon, comma
       , space
       , dot, exclamation, question, suspension
       , equal
       , pipe
       , backslash, forwardslash
       , squote, dquote
       , lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket
       ) where

import Prelude hiding (last, fail)
import Data.Function (on)
import Data.List hiding (last)
import Control.Monad (guard)

------------------------------------------------------------------------------
-- A document as a list of potential layouts

newtype Doc = Doc { runDoc :: [Layout] }

render :: Doc -> [String]
render (Doc ls) = renderL
  $ maybe (textL "") (const $ minimumBy (compare `on` height) ls)
  $ uncons ls

-- basic combinators
empty :: Doc
empty = text ""

char :: Char -> Doc
char = text . pure

text :: String -> Doc
text = Doc . filter validL . pure . textL

spaces :: Int -> Doc
spaces 0 = empty
spaces n = text (replicate n ' ')

fail :: Doc
fail = Doc []

parens :: Doc -> Doc
parens d = char '(' <> d <> char ')'

indent :: Int -> Doc -> Doc
indent n d = spaces n <> d

flush :: Doc -> Doc
flush = Doc . map flushL . runDoc

-- Combining documents

instance Semigroup Doc where
  Doc as <> Doc bs = Doc $ do
    a <- as
    b <- bs
    let ab = appendL a b
    guard (validL ab)
    pure ab

instance Monoid Doc where
  mempty = empty
  mappend = (<>)

(<+>) :: Doc -> Doc -> Doc
as <+> bs = as <> space <> bs

($$) :: Doc -> Doc -> Doc
x $$ y = flush x <> y

(<|>) :: Doc -> Doc -> Doc
Doc as <|> Doc bs = Doc (as ++ bs)

foldDoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
foldDoc _ []       = empty
foldDoc _ (x : []) = x
foldDoc f (x : xs) = f x (foldDoc f xs)

hsep :: [Doc] -> Doc
hsep = foldDoc (<+>)

vcat :: [Doc] -> Doc
vcat = foldDoc ($$)

sep :: [Doc] -> Doc
sep [] = empty
sep xs = hsep xs <|> vcat xs

commaSep :: [Doc] -> Doc
commaSep = foldDoc (\ d e -> d <> comma <+> e)

-- Special characters

-- punctuation
semi, colon, comma, space, dot, exclamation, question, suspension :: Doc
semi = char ';'
colon = char ':'
comma = char ','
space = char ' '
dot = char '.'
exclamation = char '!'
question = char '?'
suspension = text "..."

-- separators
equal, pipe :: Doc
equal = char '='
pipe = char '|'

-- slashes
backslash, forwardslash :: Doc
backslash = char '\\'
forwardslash = char '/'

-- quotes
squote, dquote :: Doc
squote = char '\''
dquote = char '"'

-- parentheses and brackets
lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket :: Doc
lparen = char '('
rparen = char ')'
langle = char '<'
rangle = char '>'
lbrace = char '{'
rbrace = char '}'
lbracket = char '['
rbracket = char ']'


------------------------------------------------------------------------------
-- Internal representation of blocks & layouts

indentS :: Int -> String -> String
indentS 0 s = s
indentS n s = replicate n ' ' ++ s

data Paragraph
  = Indent !Int Paragraph
  | Text String
  | Append Paragraph Paragraph
  | Empty

indentP :: Int -> Paragraph -> Paragraph
indentP 0 p = p
indentP n p = case p of
  Indent m p -> Indent (n + m) p
  _          -> Indent n p

renderP :: Paragraph -> [String] -> [String]
renderP p = go [] p where

  go :: String -> Paragraph -> [String] -> [String]
  go pad = \case
    Indent n p -> go (indentS n pad) p
    Text str   -> ((pad ++ str) :)
    Append p q -> go pad p . go pad q
    Empty      -> id

instance Semigroup Paragraph where
  (<>) = Append

instance Monoid Paragraph where
  mempty = Empty
  mappend = (<>)

type Block = Maybe (String, Paragraph)

renderB :: Block -> [String] -> [String]
renderB = \case
  Nothing      -> id
  Just (x, xs) -> (x :) . renderP xs

data Layout = Layout
  { height    :: !Int
  , width     :: !Int
  , lastWidth :: !Int
  , block     :: Block
  , last      :: String
  }

mkBlock :: Block -> String -> Paragraph -> Block
mkBlock (Just (x, xs)) y ys = Just (x, Append xs (Append (Text y) ys))
mkBlock Nothing        y ys = Just (y, ys)

textL :: String -> Layout
textL s = let w = length s in Layout
  { height    = 0
  , width     = w
  , lastWidth = w
  , block     = Nothing
  , last      = s
  }

flushL :: Layout -> Layout
flushL Layout{..} = Layout
  { height    = height + 1
  , width     = width
  , lastWidth = 0
  , block     = Just (maybe (last, Empty) (fmap (`Append` Text last)) block)
  , last      = ""
  }

appendL :: Layout -> Layout -> Layout
appendL l1 l2 = Layout
  { height    = height l1 + height l2
  , width     = max (width l1) (lastWidth l1 + width l2)
  , lastWidth = lastWidth l1 + lastWidth l2
  , block     = fst content
  , last      = snd content
  } where
  content = case block l2 of
    Nothing      -> (block l1, last l1 ++ last l2)
    Just (y, ys) -> let n = lastWidth l1 in
      ( mkBlock (block l1) (last l1 ++ y) (indentP n ys)
      , indentS n (last l2))

validL :: Layout -> Bool
validL l = width l <= 80

renderL :: Layout -> [String]
renderL l = renderB (block l) [last l]
