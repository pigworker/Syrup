------------------------------------------------------------------------------
-----                                                                    -----
-----     Main: Syrup Command Line                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Main where

import Language.Syrup.Opt
import Language.Syrup.Run

import System.Environment
import System.Exit

main :: IO ()
main = do
  opts <- getArgs >>= \args -> case parseOptions defaultOptions args of
    Left e -> die ("Error: " ++ e)
    Right opts -> pure opts
  src <- case filepath opts of
    Nothing -> getContents
    Just fp -> readFile fp
  putStr (syrup opts src)
