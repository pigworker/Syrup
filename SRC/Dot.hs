------------------------------------------------------------------------------
-----                                                                    -----
-----     Dot: Compiler to Dot graph format                              -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module Syrup.SRC.Dot where

import Control.Monad.State
import Data.List
import Data.Maybe
import Syrup.SRC.BigArray
import Syrup.SRC.Syn

data DotGate = DotGate
  { blackbox :: [String]
  , whitebox :: [String]
  , inputs   :: [String]
  , outputs  :: [String]
  }

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

pat :: Pat -> Dot (Path -> String)
pat (PVar x) = do
  i <- fresh
  pure $ ("N" ++) . show . extend i
pat (PCab x) = error "Not supported yet"

rhs :: Exp -> Dot (Path -> String)
rhs e = case exPat e of
  Nothing -> pat (PVar "")
  Just p  -> pat p

leftToRight :: [String] -> String
leftToRight ns@(_:_:_) = unlines $
  [ "  {"
  , "    rank = same;"
  , "    rankdir = LR;"
  , "    " ++ intercalate " -> " ns ++ " [color = white];"
  , "  }"
  ]
leftToRight _ = ""

connectInputs :: [String] -> String -> String
connectInputs [] _  = ""
connectInputs ns nm =
  "  { " ++ unwords ns ++ " } -> " ++ nm ++ ";"

connectOutputs :: String -> [String] -> String
connectOutputs _  [] = ""
connectOutputs nm ns =
  "  " ++ nm ++ " -> { " ++ unwords ns ++ " } [dir = none];"

gate :: String           -- name
     -> [Path -> String] -- inputs
     -> [Path -> String] -- ouputs
     -> Arr String (Path -> DotGate)
     -> Path -> DotGate
gate nm ins ous env p =
  let inputs   = fmap ($ p) ins
      outputs  = fmap ($ p) ous
      whitebox = []

      gateNode = "N" ++ show p
      blackbox = filter (/= "")
        [ "subgraph cluster_" ++ show p ++ " {"
        , "  " ++ gateNode ++ " [label = " ++ show nm ++ "]"
        , "  {"
        , "    node [shape = point, height = 0];"
        , "    " ++ intercalate "; " (inputs ++ outputs)
        , "  }"
        , leftToRight inputs
        , leftToRight outputs
        , connectInputs inputs gateNode
        , connectOutputs gateNode outputs
        , "}"
        ]
  in DotGate { blackbox, whitebox, inputs, outputs }

def :: Def -> Dot ()
def Stub{} = pure ()
def (Def (nm, ps) es eqns) = do
  id  <- fresh
  ins <- mapM pat ps
  ous <- mapM rhs es
  Dot $ modify $ \ s ->
    s { gates = insertArr (nm, gate nm ins ous (gates s) . extend id) (gates s) }

dotDef :: Def -> [String]
dotDef d = fromMaybe [] $ do
  st <- flip execStateT initDotSt $ runDot $ def d
  nm <- case d of { Stub{} -> Nothing; Def (nm, _) _ _ -> Just nm }
  ga <- findArr nm (gates st)
  pure $ blackbox (ga empty)

test :: IO ()
test = putStrLn $ unlines $
  dotDef (Def ("not", [PVar "x"]) [App "not" [Var "x"]] Nothing)
