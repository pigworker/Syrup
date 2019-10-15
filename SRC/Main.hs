------------------------------------------------------------------------------
-----                                                                    -----
-----     Main: Syrup Command Line                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

module Syrup.SRC.Main where

import Data.List
import Control.Arrow

import Syrup.SRC.Syn
import Syrup.SRC.Par
import Syrup.SRC.Ty
import Syrup.SRC.Chk
import Syrup.SRC.Expt
import Syrup.SRC.Sub

grokSy :: CoEnv -> [Either [String] (Source, String)] -> (CoEnv, [String])
grokSy env [] = (env, [])
grokSy env (Left ss : src) = (id *** ((ss ++) . ("" :))) (grokSy env src)
grokSy env (Right (Declaration dec@(DEC (f, _) _), s) : src) =
  (id *** ((drept ++) . (trept ++) . ("" :))) (grokSy env' src) where
    (_, trept, env') = mkComponent env (dec, s) mdef
    (drept, mdef) = case
        [ (def, s)
        | Right (Definition def@(Def (g, _) _ _), s) <- src
        , g == f
        ] of
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
      (t' , srcs) = inlineAliases t ls
      (g' , strs) = grokSy g srcs
  in ((t', g'), unlines strs)

main :: IO ()
main = do
  src <- getContents
  putStr (snd (syrup myTyEnv myCoEnv src))
