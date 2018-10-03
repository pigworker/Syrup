------------------------------------------------------------------------------
-----                                                                    -----
-----     Expt: Experiments on Syrup Programs                            -----
-----                                                                    -----
------------------------------------------------------------------------------

module Expt where

import Ty
import Va


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


------------------------------------------------------------------------------
-- generating input values from types
------------------------------------------------------------------------------

tyVas :: Ty1 -> [Va]
tyVas (Bit _)    = [B0, B1]
tyVas (Cable ts) = Bun <$> traverse tyVas ts


------------------------------------------------------------------------------
-- splicing output values from types
------------------------------------------------------------------------------

spliceVas :: [Ty2] -> [Va] -> [Va] -> [Va]
spliceVas [] _ _ = []
spliceVas (Bit T0 : ts) (v : vs) ws = v : spliceVas ts vs ws
spliceVas (Bit T1 : ts) vs (w : ws) = w : spliceVas ts vs ws
spliceVas (Cable ts' : ts) (Bun vs' : vs) (Bun ws' : ws) =
  Bun (spliceVas ts' vs' ws') : spliceVas ts vs ws

