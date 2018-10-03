{-# LANGUAGE PatternGuards #-}

module Lex where

import Data.Char

import Bwd

lexFile :: String -> [[Token]]
lexFile = fmap tokens . dentLines

tokens :: String -> [Token]
tokens = findBrackets B0 . raw

data Token
  = Spc Int      -- never two adjacent
  | Id String    -- never two adjacent
  | Num Int      -- never two adjacent
  | Sym String   -- two adjacent only if at least one is a solo symbol
  | Bracket Bracket [Token]
  | BadOpen Bracket [Token]
  | BadClose Bracket
  deriving Eq

instance Show Token where
  show (Spc n) = replicate n ' '
  show (Id x)  = x
  show (Num n) = show n
  show (Sym s) = s
  show (Bracket b ts) = o ++ foldMap show ts ++ c where (o, c) = brackets b
  show (BadOpen b ts) = fst (brackets b) ++ foldMap show ts
  show (BadClose b) = snd (brackets b)

data Bracket = Round | Square | Curly deriving (Eq, Show)
brackets :: Bracket -> (String, String)
brackets Round  = ("(",")")
brackets Square = ("[","]")
brackets Curly  = ("{","}")

openers, closers :: [(Token, Bracket)]
openers = [(Sym "(", Round),(Sym "[", Square),(Sym "{", Curly)]
closers = [(Sym ")", Round),(Sym "]", Square),(Sym "}", Curly)]

isAlphaNumU :: Char -> Bool
isAlphaNumU c = c == '_' || isAlphaNum c

unix :: String -> String
unix [] = []
unix ('\r' : '\n' : s) = '\n' : unix s
unix ('\n' : '\r' : s) = '\n' : unix s
unix ('\r' : s)        = '\n' : unix s
unix (c : s)           = c : unix s

dentLines :: String -> [String]
dentLines = dentify B0 . lines . unix where
  dentify lz [] = dump lz []
  dentify lz (l : ls) = case l of
    c : _ | not (isSpace c) -> dump lz (dentify (B0 :< l) ls)
    _ -> dentify (lz :< l) ls
  dump B0 ls = ls
  dump lz ls = concat lz : ls

raw :: String -> [Token]
raw "" = []
raw (c : s) | elem c " \t" = spaces 1 s
raw (c : s) | elem c solos = Sym [c] : raw s
raw (c : s) | isAlphaNumU c = alphanum (B0 :< c) s
raw (c : s) = symbol (B0 :< c) s

solos :: String
solos = ",;()[]{}"

spaces :: Int -> String -> [Token]
spaces i (c : s) | elem c " \t" = spaces (i + 1) s
spaces i s = Spc i : raw s

alphanum :: Bwd Char -> String -> [Token]
alphanum cz (c : s) | isAlphaNumU c = alphanum (cz :< c) s
alphanum cz s | all isDigit cz = Num (read (cz <>> [])) : raw s
              | otherwise = Id (cz <>> []) : raw s

symbol :: Bwd Char -> String -> [Token]
symbol cz (c : s) | not (or ([isSpace, isAlphaNum, (`elem` solos)] <*> [c]))
  = symbol (cz :< c) s
symbol cz s = Sym (cz <>> []) : raw s

findBrackets :: Bwd (Bracket, Bwd Token) -> [Token] -> [Token]
findBrackets B0 [] = []
findBrackets (bz :< (b, tz)) [] = findBrackets bz [BadOpen b (tz <>> [])]
findBrackets bz (t : ts) | Just b <- lookup t openers = findBrackets (bz :< (b, B0)) ts
findBrackets bz (t : ts) | Just c <- lookup t closers = case bz of
  bz' :< (b, tz) | b == c -> findBrackets bz' (Bracket b (tz <>> []) : ts)
  _ -> findBrackets bz (BadClose c : ts)
findBrackets (bz :< (b, tz)) (t : ts) = findBrackets (bz :< (b, tz :< t)) ts
findBrackets B0 (t : ts) = t : findBrackets B0 ts
