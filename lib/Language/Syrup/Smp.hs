------------------------------------------------------------------------------
-----                                                                    -----
-----     Smp: Sample programs to run tests on                           -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Smp where

import Language.Syrup.Syn
import Language.Syrup.Ty

zero :: TypedDef
zero = let ty = Bit Unit in
  Def ("zero", []) [Var ty "D"] $ Just $
  [ ([PVar ty "D"] :=: [App [ty] "dff" [Var ty "E"]])
  , ([PVar ty "E"] :=: [App [ty] "nand" [Var ty "F", Var ty "F"]])
  , ([PVar ty "F"] :=: [App [ty] "nand" [Var ty "D", Var ty "G"]])
  , ([PVar ty "G"] :=: [App [ty] "nand" [Var ty "D", Var ty "D"]])
  ]

nand :: TypedDef
nand = let ty = Bit Unit in
  Def ("nand", PVar ty <$> ["X", "Y"]) [Var ty "Z"] $ Just $
  [ ([PVar ty "Z"] :=: [App [ty] "nand" (Var ty <$> ["X", "Y"])]) ]

notG :: TypedDef
notG = let ty = Bit Unit in
  Def ("not", [PVar ty "X"]) [Var ty "Z"] $ Just $
  [ [PVar ty "Z"] :=: [App [ty] "nand" [Var ty "X", Var ty "X"]] ]

andG :: TypedDef
andG = let ty = Bit Unit in
  Def ("and", [PVar ty "x", PVar ty "y"])
           [App [ty] "not" [App [ty] "nand" [Var ty "x", Var ty "y"]]]
           Nothing

orG :: TypedDef
orG = let ty = Bit Unit in
  Def ("or", PVar ty <$> ["X", "Y"]) [Var ty "Z"] $ Just $
  [ ([PVar ty "Z"] :=: [App [ty] "nand" (App [ty] "not" . pure . Var ty <$> ["X", "Y"])]) ]

dff :: TypedDef
dff = let ty = Bit Unit in
  Def ("dff", [PVar ty "D"]) [Var ty "Q"] $ Just $
  [ ([PVar ty "Q"] :=: [App [ty] "dff" [Var ty "D"]]) ]

xor :: TypedDef
xor = let ty = Bit Unit in
  Def ("xor", PVar ty <$> ["X", "Y"]) [App [ty] "or" (Var ty <$> ["A", "B"])] $ Just
  [ ([PVar ty "A"]  :=: [App [ty] "and" (Var ty <$> ["Y", "X"])])
  , ([PVar ty "B"]  :=: [App [ty] "and" (Var ty <$> ["NX", "NY"])])
  , ([PVar ty "NX"] :=: [App [ty] "not" [Var ty "X"]])
  , ([PVar ty "NY"] :=: [App [ty] "not" [Var ty "Y"]])
  ]

tff :: TypedDef
tff = let ty = Bit Unit in
  Def ("tff", [PVar ty "T"]) [Var ty "Q"] $ Just $
  [ ([PVar ty "D"] :=: [App [ty] "xor" [Var ty "Q", Var ty "T"]])
  , ([PVar ty "Q"] :=: [App [ty] "dff" [Var ty "D"]])
  ]

foo :: TypedDef
foo = let ty = Bit Unit in
  Def ("foo", PVar ty <$> ["A", "B", "C"])
      ([App [ty] "and" [Var ty "A", Var ty "B"], Var ty "Z"])
      $ Just [([PVar ty "Z"] :=: [App [ty] "or" [Var ty "A"
                                 , App [ty] "and" [Var ty "B", Var ty "C"]]])]

and4 :: TypedDef
and4 = let ty = Bit Unit in
  Def ("and4", PVar ty <$> ["A", "B", "C", "D"])
           [foldr1 (\ a b -> App [ty] "and" [a, b]) $ Var ty <$> ["A", "B", "C", "D"]]
           Nothing

and4' :: TypedDef
and4' = let ty = Bit Unit in
  Def ("and4'", PVar ty <$> ["A", "B", "C", "D"])
  [App [ty] "and" [ App [ty] "and" (Var ty <$> ["A", "B"])
             , App [ty] "and" (Var ty <$> ["C", "D"])
             ]
  ]
  Nothing

swapG :: TypedDef
swapG = let ty = Bit Unit in
  Def ("swap", [PVar ty "x", PVar ty "y"]) [Var ty "y", Var ty "x"] Nothing

mux :: TypedDef
mux = let ty = Bit Unit in
  Def ("mux", PVar ty <$> ["C", "X", "Y"])
    [ App [ty] "or" [ App [ty] "and" [App [ty] "not" [Var ty "C"], Var ty "X"]
               , App [ty] "and" (Var ty <$> ["C", "Y"])
               ]
    ] Nothing

mux2 :: TypedDef
mux2 = let ty = Bit Unit in
  Def ("mux", PVar ty <$> ["C", "X1", "X2", "Y1", "Y2"])
      (Var ty <$> ["A", "B"]) $ Just
      [ ([PVar ty "A"] :=: [App [ty] "mux" (Var ty <$> [ "C", "X1", "Y1" ])])
      , ([PVar ty "B"] :=: [App [ty] "mux" (Var ty <$> [ "C", "X2", "Y2" ])])
      ]

hadd :: TypedDef
hadd = let ty = Bit Unit in
  Def ("hadd", PVar ty <$> ["X", "Y"])
      [ App [ty] "and" (Var ty <$> ["X", "Y"])
      , App [ty] "xor" (Var ty <$> ["X", "Y"])
      ]
      Nothing

fadd :: TypedDef
fadd = let ty = Bit Unit in
  Def ("fadd", PVar ty <$> ["X", "Y", "C"])
      (Var ty <$> ["C1", "Z0"]) $ Just
      [ ((PVar ty <$> ["CA", "D"])  :=: [App [ty, ty] "hadd" (Var ty <$> ["X", "Y"])])
      , ((PVar ty <$> ["CB", "Z0"]) :=: [App [ty, ty] "hadd" (Var ty <$> ["D", "C"])])
      , ([PVar ty "C1"]             :=: [App [ty] "xor"  (Var ty <$> ["CA", "CB"])])
      ]

rca4 :: TypedDef
rca4 = let ty = Bit Unit in
  Def ("rca4", PVar ty <$> ["X3", "X2", "X1", "X0", "Y3", "Y2", "Y1", "Y0", "CI"])
      (Var ty <$> ["CO", "Z3", "Z2", "Z1", "Z0"]) $ Just
      [ ((PVar ty <$> ["CO", "Z3"]) :=: [App [ty, ty] "fadd" (Var ty <$> [ "X3", "Y3", "C3" ])])
      , ((PVar ty <$> ["C3", "Z2"]) :=: [App [ty, ty] "fadd" (Var ty <$> [ "X2", "Y2", "C2" ])])
      , ((PVar ty <$> ["C2", "Z1"]) :=: [App [ty, ty] "fadd" (Var ty <$> [ "X1", "Y1", "C1" ])])
      , ((PVar ty <$> ["C1", "Z0"]) :=: [App [ty, ty] "fadd" (Var ty <$> [ "X0", "Y0", "CI" ])])
      ]

andnot :: TypedDef
andnot = let ty = Bit Unit in
  Def ("andnot", [PVar ty "X"]) [Var ty "R"] $ Just
    [ [PVar ty "Z"] :=: [App [ty] "not" [Var ty "X"]]
    , [PVar ty "R"] :=: [App [ty] "and" (Var ty <$> ["Z", "Z"])]
    ]
