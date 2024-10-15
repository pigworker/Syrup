module Utilities.HTML where

import Data.List (intersperse, intercalate)

htmlc :: Char -> String
htmlc '<' = "&lt;"
htmlc '>' = "&gt;"
htmlc '&' = "&amp;"
htmlc c = [c]

escapeHTML :: String -> String
escapeHTML = concatMap htmlc

br :: String
br = "<br />"

asHTML :: [String] -> String
asHTML ls = intercalate "\n" $ intersperse br (escapeHTML <$> ls)

tag :: String -> String -> String
tag t txt = concat ["<",t,">",txt,"</",t,">"]

pre :: String -> String
pre = tag "pre"

code :: String -> String
code = tag "code"

span :: [String] -> String -> String
span attrs doc = concat
      [ "<span "
      , unwords attrs
      , ">"
      , doc
      , "</span>"
      ]

div :: [String] -> String -> String
div attrs doc = concat
      [ "<div "
      , unwords attrs
      , ">"
      , doc
      , "</div>"
      ]
