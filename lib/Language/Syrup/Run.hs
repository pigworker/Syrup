------------------------------------------------------------------------------
-----                                                                    -----
-----     Run: Running Syrup on a default environment                    -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Run where

import Control.Arrow ((***))
import Data.Functor (void)
import Data.List (intercalate)

import Language.Syrup.Chk
import Language.Syrup.Dot
import Language.Syrup.Expt
import Language.Syrup.Lnt
import Language.Syrup.Par
import Language.Syrup.Scp
import Language.Syrup.Sub
import Language.Syrup.Syn
import Language.Syrup.Ty
import Language.Syrup.Utils

getDefsOf :: String -> [Either [String] (Source, String)] -> [(Def, String)]
getDefsOf f src = src >>= \case
  Right (Definition def@(Def (g, _) _ _), s) | g == f -> [(def,s)]
  Right (Definition def@(Stub g _)      , s) | g == f -> [(def,s)]
  _ -> []

grokSy :: (CoEnv, DotSt)
       -> [Either [String] (Source, String)]
       -> ((CoEnv, DotSt), [String])
grokSy env [] = (env, [])
grokSy env (Left ss : src) = (id *** ((ss ++) . ("" :))) (grokSy env src)
grokSy (env, st) (Right (Declaration dec@(DEC (f, _) _), s) : src) =
  (id *** ((concatMap (++ [""]) warn ++) . (drept ++) . (trept ++) . ("" :)))
  (grokSy (env', st) rest) where
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
grokSy (env, st) (Right (Definition d, _) : src) =
  let st' = addDef st d in grokSy (env, st') src

runSyrup :: (TyEnv, (CoEnv, DotSt))
         -> String
         -> ((TyEnv, (CoEnv, DotSt)), String)
runSyrup (t, (g, st)) txt =
  let ls          = syrupFile txt
      linted      = linter ls
      scps        = check (globalScope (void g)) linted
      (t' , srcs) = inlineAliases t scps
      (g' , strs) = grokSy (g, st) srcs
  in ((t', g'), unlines strs)

syrup :: String -> String
syrup src = snd (runSyrup (myTyEnv, (myCoEnv, myDotSt)) src)
