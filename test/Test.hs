module Main where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Traversable (for)

import System.FilePath (takeBaseName, replaceExtension)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

import Language.Syrup.Run
import Language.Syrup.Opt (defaultOptions)

main :: IO ()
main = do
 sources <- findByExtension [".syrup"] "test"
 let mkTest = \ src ->
      let testName = takeBaseName src in
      let goldenFile = replaceExtension src "out" in
      goldenVsString testName goldenFile $ do
        txt <- readFile src
        pure $ pack $ syrup defaultOptions txt
 defaultMain $ testGroup "Tests" (mkTest <$> sources)
