------------------------------------------------------------------------------
-----                                                                    -----
-----     Main: Syrup Command Line                                       -----
-----                                                                    -----
------------------------------------------------------------------------------

module Main where

import Data.List

import Syn
import Par
import Ty
import Chk
import Expt

grok :: CoEnv -> [Either [String] (Source, String)] -> [String]
grok env [] = []
grok env (Left ss : src) = ss ++ "" : grok env src
grok env (Right (Declaration dec@(DEC (f, _) _), s) : src) =
  drept ++ trept ++ [""] ++ grok env' src where
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
  experiment env expt ++ "" : grok env src
grok env (Right (Definition _, _) : src) = grok env src

main :: IO ()
main = do
  src <- syrupFile <$> getContents
  foldMap putStrLn (grok myCoEnv src)