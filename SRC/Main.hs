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

grok :: CoEnv -> [Either [String] (Source, String)] -> (CoEnv, [String])
grok env [] = (env, [])
grok env (Left ss : src) = (id *** ((ss ++) . ("" :))) (grok env src)
grok env (Right (Declaration dec@(DEC (f, _) _), s) : src) =
  (id *** ((drept ++) . (trept ++) . ("" :))) (grok env' src) where
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
grok env (Right (Experiment expt, _) : src) =
  (id *** ((experiment env expt ++) . ("" :))) (grok env src)
grok env (Right (Definition _, _) : src) = grok env src

syrup :: CoEnv -> String -> (CoEnv, String)
syrup g = (id *** unlines) . grok g . syrupFile

main :: IO ()
main = do
  src <- getContents
  putStr (snd (syrup myCoEnv src))