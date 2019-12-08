------------------------------------------------------------------------------
-----                                                                    -----
-----     Dot: Compiler to Dot graph format                              -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}

module Syrup.SRC.Dot where

import Control.Monad.State
import Data.List
import Data.Maybe

import Syrup.SRC.BigArray
import Syrup.SRC.Syn
import Syrup.SRC.Anf
  ( Input'(..), Input
  , Output'(..), Output
  , Gate(..)
  , toGate
  )

data DotGate = DotGate
  { blackbox    :: [String]
  , whitebox    :: [String]
  , inputNodes  :: [String]
  , outputNodes :: [String]
  }

indent :: Int -> String -> String
indent n str = replicate n ' ' ++ str

-- Path of unique IDs
newtype Path = Path { getPath :: [Int] }

empty :: Path
empty = Path []

extend :: Int -> Path -> Path
extend i (Path is) = Path (i : is)

instance Show Path where
  show (Path ps) = intercalate "-" (show <$> reverse ps)

data DotSt = DotSt
  { supply :: Int
  , gates  :: Arr String (Path -> DotGate)
  }

initDotSt :: DotSt
initDotSt = DotSt
  { supply = 0
  , gates  = emptyArr
  }

newtype Dot a = Dot { runDot :: StateT DotSt Maybe a }
  deriving (Functor, Applicative, Monad)

evalDot :: Dot a -> Maybe a
evalDot = flip evalStateT initDotSt . runDot

fresh :: Dot Int
fresh = Dot $ do
  s <- get
  let i = supply s
  put $ s { supply = i + 1 }
  pure i

leftToRight :: String -> [String] -> String
leftToRight str ns@(_:_:_) = unlines $ indent 2 <$>
  [ "// Order " ++ str ++ " left to right"
  , "{"
  , "  rank = same;"
  , "  rankdir = LR;"
  , "  " ++ intercalate " -> " ns ++ " [color = white];"
  , "}"
  ]
leftToRight _ _ = ""

connectInputs :: [String] -> String -> String
connectInputs [] _  = ""
connectInputs ns nm = unlines $ indent 2 <$>
  [ "// Connect inputs to gate"
  , "{ " ++ unwords ns ++ " } -> " ++ nm ++ ";"
  ]

connectOutputs :: String -> [String] -> String
connectOutputs _  [] = ""
connectOutputs nm ns = unlines $ indent 2 <$>
  [ "// Connect outputs to gate"
  , nm ++ " -> { " ++ unwords ns ++ " } [dir = none];"
  ]

mkNode :: Path -> String -> String
mkNode p str = "NODE_" ++ str ++ "_" ++ show p

mkGate :: Path -> String -> String
mkGate p str = "GATE_" ++ str ++ "_" ++ show p

declareInput :: Path -> Input -> String
declareInput p Input{..} =
  mkNode p inputName ++ " [label = " ++ inputName ++ "];"

declareOutput :: Path -> Output -> String
declareOutput p Output{..} =
  let name = mkNode p outputName in
  if isVirtual then name ++ ";" else (name ++ " [label = " ++ outputName ++ "];")

gate :: String -> Gate -> Arr String (Path -> DotGate)
     -> Path -> DotGate
gate nm Gate{..} env p =
  let gateNode    = mkGate p nm
      inputNodes  = map (mkNode p . inputName)  inputs
      outputNodes = map (mkNode p . outputName) outputs

      blackbox    = filter (/= "")
        [ "subgraph cluster_" ++ show p ++ " {"
        , "  style = invis;"
        , indent 2 $ gateNode ++ " [label = " ++ show nm ++ "];"
        , "  {"
        , "    node [shape = point, height = 0];"
        ,      unlines (map (indent 4 . declareInput p) inputs)
            ++ unlines (map (indent 4 . declareOutput p) outputs)
        , "  }"
        , leftToRight "inputs" inputNodes
        , leftToRight "outputs" outputNodes
        , connectInputs inputNodes gateNode
        , connectOutputs gateNode outputNodes
        , "}"
        ]

      whitebox = []
  in DotGate { blackbox, whitebox, inputNodes, outputNodes }

def :: Def -> Dot ()
def d = case toGate d of
  Nothing      -> pure ()
  Just (nm, g) -> do
    id <- fresh
    Dot $ modify $ \ s ->
      s { gates = insertArr (nm, gate nm g (gates s) . extend id) (gates s) }

blackBoxDef :: Def -> [String]
blackBoxDef d = fromMaybe [] $ do
  st <- flip execStateT initDotSt $ runDot $ def d
  nm <- case d of { Stub{} -> Nothing; Def (nm, _) _ _ -> Just nm }
  ga <- findArr nm (gates st)
  pure $
    [ "digraph blackbox {"
    , "  rankdir=TB;"
    , "  splines=ortho;"
    ]
    ++ blackbox (ga empty)
    ++ ["}"]


notG :: Def
notG = Def ("not", [PVar "x"]) [App "not" [Var "x"]] Nothing

andG :: Def
andG = Def ("and", [PVar "x", PVar "y"]) [Var "z"] $ Just
  [ [PVar "z"] :=: [App "and" [Var "x", Var "y"]]
  ]

test :: IO ()
test = do
  let runner = putStrLn . unlines . blackBoxDef
  runner notG
  runner andG
