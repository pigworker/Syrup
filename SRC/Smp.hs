------------------------------------------------------------------------------
-----                                                                    -----
-----     Smp: Sample programs to run tests on                           -----
-----                                                                    -----
------------------------------------------------------------------------------

module Syrup.SRC.Smp where

import Syrup.SRC.Syn

foo :: Def
foo =
  Def ("foo", PVar <$> ["A", "B", "C"])
      ([App "and" [Var "A", Var "B"], Var "Z"])
      $ Just [([PVar "Z"] :=: [App "or" [Var "A"
                                        , App "and" [Var "B", Var "C"]]])]

and4 :: Def
and4 = Def ("and4", PVar <$> ["A", "B", "C", "D"])
           [foldr1 (\ a b -> App "and" [a, b]) $ Var <$> ["A", "B", "C", "D"]]
           Nothing

and4' :: Def
and4' =
  Def ("and4'", PVar <$> ["A", "B", "C", "D"])
  [App "and" [ App "and" (Var <$> ["A", "B"])
             , App "and" (Var <$> ["C", "D"])
             ]
  ]
  Nothing


notG :: Def
notG = Def ("not", [PVar "x"]) [App "not" [Var "x"]] Nothing

andG :: Def
andG = Def ("and", [PVar "x", PVar "y"]) [Var "z"] $ Just
  [ [PVar "z"] :=: [App "and" [Var "x", Var "y"]]
  ]

swapG :: Def
swapG = Def ("swap", [PVar "x", PVar "y"]) [Var "y", Var "x"] Nothing
