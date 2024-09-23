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
  src <- getContents
  opts <- getArgs >>= \args -> case parseOptions args of
    Left e -> die ("Error: " ++ e)
    Right opts -> pure opts
  putStr (syrup opts src)
