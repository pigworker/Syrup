module Main where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Traversable (for)

import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeBaseName, replaceExtension)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

import Language.Syrup.Run
import Language.Syrup.Opt (defaultOptions, parseOptions)

main :: IO ()
main = do
 sources <- findByExtension [".syrup"] "test"
 let mkTest = \ src ->
      let testName = takeBaseName src in
      let goldenFile = replaceExtension src "out" in
      let flagsFile = replaceExtension src "flags" in
      goldenVsString testName goldenFile $ do
        txt <- readFile src
        opts <- do
          check <- doesFileExist flagsFile
          flags <- if check then words <$> readFile flagsFile else pure []
          case parseOptions defaultOptions flags of
            Left err -> die err
            Right opts -> pure opts
        pure $ pack $ syrup opts txt
 defaultMain $ testGroup "Tests" (mkTest <$> sources)
