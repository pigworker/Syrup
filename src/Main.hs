------------------------------------------------------------------------------
-----                                                                    -----
-----     Main: Syrup Command Line                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow
import Data.Functor
import Data.List

import Language.Syrup.Syn
import Language.Syrup.Par
import Language.Syrup.Ty
import Language.Syrup.Chk
import Language.Syrup.Expt
import Language.Syrup.Sub
import Language.Syrup.Scp
import Language.Syrup.Lnt
import Language.Syrup.Utils

getDefsOf :: String -> [Either [String] (Source, String)] -> [(Def, String)]
getDefsOf f src = src >>= \case
  Right (Definition def@(Def (g, _) _ _), s) | g == f -> [(def,s)]
  Right (Definition def@(Stub g _)      , s) | g == f -> [(def,s)]
  _ -> []

grokSy :: CoEnv -> [Either [String] (Source, String)] -> (CoEnv, [String])
grokSy env [] = (env, [])
grokSy env (Left ss : src) = (id *** ((ss ++) . ("" :))) (grokSy env src)
grokSy env (Right (Declaration dec@(DEC (f, _) _), s) : src) =
  (id *** ((concatMap (++ [""]) warn ++) . (drept ++) . (trept ++) . ("" :))) (grokSy env' rest) where
    (_, trept, env') = mkComponent env (dec, s) mdef
    (warn, rest)  = spanMaybe isLeft src
    (drept, mdef) = case getDefsOf f rest of
      [defs] -> ([], Just defs)
      zs@(_ : _ : _) ->
        (["I don't know which of the following is your preferred "
          ++ f ++ ":"]
         ++ intercalate [""] (map (lines . snd) zs) ++ [""]
        , Nothing)
      [] -> (["You haven't defined " ++ f ++ " just now.", ""], Nothing)
grokSy env (Right (Experiment expt, _) : src) =
  (id *** ((experiment env expt ++) . ("" :))) (grokSy env src)
grokSy env (Right (Definition _, _) : src) = grokSy env src

syrup :: TyEnv -> CoEnv -> String -> ((TyEnv, CoEnv), String)
syrup t g txt =
  let ls          = syrupFile txt
      linted      = linter ls
      scps        = check (globalScope (void g)) linted
      (t' , srcs) = inlineAliases t scps
      (g' , strs) = grokSy g srcs
  in ((t', g'), unlines strs)

main :: IO ()
main = do
  src <- getContents
  putStr (snd (syrup myTyEnv myCoEnv src))
