------------------------------------------------------------------------------
-----                                                                    -----
-----     Expt: Experiments on Syrup Programs                            -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Expt where

import Data.List

import BigArray
import Syn
import Ty
import Va


------------------------------------------------------------------------------
-- experiments
------------------------------------------------------------------------------

experiment :: CoEnv -> EXPT -> [String]
experiment g (Tabulate x) = case findArr x g of
  Nothing -> ["I don't know what " ++ x ++ "is."]
  Just c ->  ["Truth table for " ++ x ++ ":"] ++
             displayTab (tabCompo c)
experiment g (Simulate x m0 iss) = case findArr x g of
  Nothing -> ["I don't know what " ++ x ++ "is."]
  Just c ->  ["Simulation for " ++ x ++ ":"] ++
             runCompo c m0 iss


------------------------------------------------------------------------------
-- running tine sequences
------------------------------------------------------------------------------

runCompo :: Compo -> [Va] -> [[Va]] -> [String]
runCompo c m0 iss
  | not (tyVaChks mTys m0)
  = [ concat ["Memory for ", monick c, " has type {",
              csepShow mTys, "}"]
    , concat ["That can't store {", foldMap show m0, "}."]
    ]
  | Just is <- find (not . tyVaChks iTys) iss
  = [ concat ["Inputs for ", monick c, " are typed ",
              csepShow iTys, ""]
    , concat ["That can't accept ", foldMap show is, "."]
    ]
  | otherwise = render (go 0 m0 iss)
  where
    mTys = memTys c
    iTys = inpTys c
    oTys = oupTys c
    go t mt [] = ([], (t, mt))
    go t mt (is : iss) = ((t, mt, is, os) : xs , (z, mo)) where
      (xs, (z, mo)) = go (t + 1) mt' iss
      ou0 = stage0 c mt
      mou1 = stage1 c (mt ++ is)
      (mt', ou1) = splitAt (length mTys) mou1
      os = spliceVas oTys ou0 ou1
    render (xs, (z, mo)) = map row xs ++ [lastrow]
      where
        w = length (show z)
        showtime t = reverse . take w  $ reverse (show t) ++ repeat ' '
        row (t, m, is, os) = concat
          [ showtime t, " {", foldMap show m, "} "
          , foldMap show is, " -> ", foldMap show os
          ]
        lastrow = concat
          [ showtime z, " {", foldMap show mo, "}" ]
  


tyVaChks :: [Ty1] -> [Va] -> Bool
tyVaChks ts vs = length ts == length vs && all id (zipWith tyVaChk ts vs)

tyVaChk :: Ty1 -> Va -> Bool
tyVaChk (Bit ()) V0 = True
tyVaChk (Bit ()) V1 = True
tyVaChk (Cable ts) (VC vs) = tyVaChks ts vs
tyVaChk _ _ = False


------------------------------------------------------------------------------
-- tabulating behaviours of components
------------------------------------------------------------------------------

tabCompo :: Compo -> [( [Va]  -- inputs in
                      , [( [Va]  -- memory in
                         , ( [Va]  -- memory out
                           , [Va]  -- outputs out
                           ))])]
tabCompo c =
  [ (ii, [ (mi, spl ou0 (stage1 c (mi ++ ii)))
         | (mi, ou0) <- meTab
         ] )
  | ii <- inTab
  ]
  where
    inTab = traverse tyVas (inpTys c)
    meTab = [(mi, stage0 c mi) | mi <- traverse tyVas (memTys c)]
    spl ou0 mou1 = (mo, spliceVas (oupTys c) ou0 ou1) where
      (mo, ou1) = splitAt (length (memTys c)) mou1

displayTab :: [( [Va]  -- inputs in
                      , [( [Va]  -- memory in
                         , ( [Va]  -- memory out
                           , [Va]  -- outputs out
                           ))])]
           -> [String]
displayTab = (>>= go) where
  go (is, [([],([], os))]) = [foldMap show is ++ " | " ++ foldMap show os]
  go (is, xs) = zipWith (++)
      (l : repeat (replicate (length l) ' '))
      [ concat [" { ", foldMap show mi, " -> ", foldMap show mo, " } ",
                foldMap show os]
      | (mi, (mo, os)) <- xs
      ]
    where l = foldMap show is


------------------------------------------------------------------------------
-- generating input values from types
------------------------------------------------------------------------------

tyVas :: Ty1 -> [Va]
tyVas (Bit _)    = [V0, V1]
tyVas (Cable ts) = VC <$> traverse tyVas ts


------------------------------------------------------------------------------
-- splicing output values from types
------------------------------------------------------------------------------

spliceVas :: [Ty2] -> [Va] -> [Va] -> [Va]
spliceVas [] _ _ = []
spliceVas (Bit T0 : ts) (v : vs) ws = v : spliceVas ts vs ws
spliceVas (Bit T1 : ts) vs (w : ws) = w : spliceVas ts vs ws
spliceVas (Cable ts' : ts) (VC vs' : vs) (VC ws' : ws) =
  VC (spliceVas ts' vs' ws') : spliceVas ts vs ws

