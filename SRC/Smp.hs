------------------------------------------------------------------------------
-----                                                                    -----
-----     Smp: Sample programs to run tests on                           -----
-----                                                                    -----
------------------------------------------------------------------------------

module Syrup.SRC.Smp where

import Syrup.SRC.Syn

nand :: Def
nand =
  Def ("nand", PVar <$> ["X", "Y"]) [Var "Z"] $ Just $
  [ ([PVar "Z"] :=: [App "nand" (Var <$> ["X", "Y"])]) ]

notG :: Def
notG = Def ("not", [PVar "X"]) [Var "Z"] $ Just $
  [ [PVar "Z"] :=: [App "nand" [Var "X", Var "X"]] ]

andG :: Def
andG = Def ("and", [PVar "x", PVar "y"])
           [App "not" [App "nand" [Var "x", Var "y"]]]
           Nothing

orG :: Def
orG = Def ("or", PVar <$> ["X", "Y"]) [Var "Z"] $ Just $
  [ ([PVar "Z"] :=: [App "nand" (App "not" . pure . Var <$> ["X", "Y"])]) ]

dff :: Def
dff =
  Def ("dff", [PVar "D"]) [Var "Q"] $ Just $
  [ ([PVar "Q"] :=: [App "dff" [Var "D"]]) ]

xor :: Def
xor =
  Def ("xor", PVar <$> ["X", "Y"]) [App "or" (Var <$> ["A", "B"])] $ Just
  [ ([PVar "A"]  :=: [App "and" (Var <$> ["Y", "X"])])
  , ([PVar "B"]  :=: [App "and" (Var <$> ["NX", "NY"])])
  , ([PVar "NX"] :=: [App "not" [Var "X"]])
  , ([PVar "NY"] :=: [App "not" [Var "Y"]])
  ]

tff :: Def
tff =
  Def ("tff", [PVar "T"]) [Var "Q"] $ Just $
  [ ([PVar "D"] :=: [App "xor" [Var "Q", Var "T"]])
  , ([PVar "Q"] :=: [App "dff" [Var "D"]])
  ]

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

swapG :: Def
swapG = Def ("swap", [PVar "x", PVar "y"]) [Var "y", Var "x"] Nothing

mux :: Def
mux = Def ("mux", PVar <$> ["C", "X", "Y"])
    [ App "or" [ App "and" [App "not" [Var "C"], Var "X"]
               , App "and" (Var <$> ["C", "Y"])
               ]
    ] Nothing

mux2 :: Def
mux2 =
  Def ("mux", PVar <$> ["C", "X1", "X2", "Y1", "Y2"])
      (Var <$> ["A", "B"]) $ Just
      [ ([PVar "A"] :=: [App "mux" (Var <$> [ "C", "X1", "Y1" ])])
      , ([PVar "B"] :=: [App "mux" (Var <$> [ "C", "X2", "Y2" ])])
      ]

hadd :: Def
hadd =
  Def ("hadd", PVar <$> ["X", "Y"])
      [ App "and" (Var <$> ["X", "Y"])
      , App "xor" (Var <$> ["X", "Y"])
      ]
      Nothing

fadd :: Def
fadd =
  Def ("fadd", PVar <$> ["X", "Y", "C"])
      (Var <$> ["C1", "Z0"]) $ Just
      [ ((PVar <$> ["CA", "D"])  :=: [App "hadd" (Var <$> ["X", "Y"])])
      , ((PVar <$> ["CB", "Z0"]) :=: [App "hadd" (Var <$> ["D", "C"])])
      , ([PVar "C1"]             :=: [App "xor"  (Var <$> ["CA", "CB"])])
      ]

rca4 :: Def
rca4 =
  Def ("rca4", PVar <$> ["X3", "X2", "X1", "X0", "Y3", "Y2", "Y1", "Y0", "CI"])
      (Var <$> ["CO", "Z3", "Z2", "Z1", "Z0"]) $ Just
      [ ((PVar <$> ["CO", "Z3"]) :=: [App "fadd" (Var <$> [ "X3", "Y3", "C3" ])])
      , ((PVar <$> ["C3", "Z2"]) :=: [App "fadd" (Var <$> [ "X2", "Y2", "C2" ])])
      , ((PVar <$> ["C2", "Z1"]) :=: [App "fadd" (Var <$> [ "X1", "Y1", "C1" ])])
      , ((PVar <$> ["C1", "Z0"]) :=: [App "fadd" (Var <$> [ "X0", "Y0", "CI" ])])
      ]

andnot :: Def
andnot =
  Def ("andnot", [PVar "X"]) [Var "R"] $ Just
    [ [PVar "Z"] :=: [App "not" [Var "X"]]
    , [PVar "R"] :=: [App "and" (Var <$> ["Z", "Z"])]
    ]
