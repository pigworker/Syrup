------------------------------------------------------------------------------
-----                                                                    -----
-----     Main: Syrup Command Line                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Syrup.SRC.Main where

import Data.List
import Control.Arrow
import Control.Monad
import Data.Functor

import Syrup.SRC.Syn
import Syrup.SRC.Par
import Syrup.SRC.Ty
import Syrup.SRC.Chk
import Syrup.SRC.Expt
import Syrup.SRC.Sub
import Syrup.SRC.Scp

getDefsOf :: String -> [Either [String] (Source, String)] -> [(Def, String)]
getDefsOf f src = src >>= \case
  Right (Definition def@(Def (g, _) _ _), s) | g == f -> [(def,s)]
  Right (Definition def@(Stub g _)      , s) | g == f -> [(def,s)]
  _ -> []

grokSy :: CoEnv -> [Either [String] (Source, String)] -> (CoEnv, [String])
grokSy env [] = (env, [])
grokSy env (Left ss : src) = (id *** ((ss ++) . ("" :))) (grokSy env src)
grokSy env (Right (Declaration dec@(DEC (f, _) _), s) : src) =
  (id *** ((drept ++) . (trept ++) . ("" :))) (grokSy env' src) where
    (_, trept, env') = mkComponent env (dec, s) mdef
    (drept, mdef) = case getDefsOf f src of
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
      scps        = check (globalScope (void g)) ls
      (t' , srcs) = inlineAliases t scps
      (g' , strs) = grokSy g srcs
  in ((t', g'), unlines strs)

main :: IO ()
main = do
  src <- getContents
  putStr (snd (syrup myTyEnv myCoEnv src))
