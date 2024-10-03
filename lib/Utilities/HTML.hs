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

pre :: String -> String
pre txt = "<pre>" ++ txt ++ "</pre>"

span :: [String] -> String -> String
span attrs doc = concat
      [ "<span "
      , unwords attrs
      , ">"
      , doc
      , "</span>"
      ]
