------------------------------------------------------------------------------
-----                                                                    -----
-----     Main: Syrup Command Line                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Main where

import Language.Syrup.Run

main :: IO ()
main = do
  src <- getContents
  putStr (syrup src)
