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

import Control.Monad.Writer
import Control.Monad.State
import Data.List
import Data.Maybe

import Syrup.SRC.BigArray
import Syrup.SRC.Syn
import Syrup.SRC.Anf
  ( Input'(..), Input
  , Output'(..), Output
  , Expr'(..)
  , Gate(..)
  , toGate
  , evalFresh
  , Fresh
  )
import qualified Syrup.SRC.Anf as Anf

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
  show (Path ps) = intercalate "__" (show <$> reverse ps)

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

freshId :: Dot Int
freshId = Dot $ do
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
mkNode p str = show $ "NODE_" ++ str ++ "_" ++ show p

mkGate :: Path -> String -> String
mkGate p str = show $ "GATE_" ++ str ++ "_" ++ show p

mkArrow :: Path -> String -> String -> String
mkArrow p x y = arrow True (mkNode p x) (mkNode p y)

arrow :: Bool -> String -> String -> String
arrow b x y = concat
  [ x
  , " -> "
  , y
  , if b then ";" else " [dir = none];"
  ]

declareInput :: Path -> Input -> String
declareInput p Input{..} =
  mkNode p inputName ++ " [label = " ++ show inputName ++ "];"

declareOutput :: Path -> Output -> String
declareOutput p Output{..} =
  let name = mkNode p outputName in
  name ++ " [label = " ++ (if isVirtual then show "" else show outputName) ++ "];"

declareLocal :: Path -> Output -> String
declareLocal p Output{..} =
  let name = mkNode p outputName in
  name ++ " [shape = point, height = 0];"


-- The monad we use to generate the content of the whitebox implementation:
-- we need a name supply & we would rather use Writer to collect the lines
-- of code rather than having to assemble everything by hand.
newtype WhiteBox a = WhiteBox { runWhiteBox :: WriterT [String] Fresh a }
  deriving (Functor, Applicative, Monad, MonadWriter [String])

execWhiteBox :: WhiteBox () -> [String]
execWhiteBox = evalFresh . execWriterT . runWhiteBox

fresh :: WhiteBox Int
fresh = WhiteBox $ lift Anf.fresh

nodeCluster :: Path -> String -> [String] -> [String]
nodeCluster p name nodes =
  [ "  subgraph cluster_" ++ name ++ "_" ++ show p ++ " {"
  , "    style = invis;"
  , "    node [shape = none];"
  ,      unlines $ map (indent 4) nodes
  , "  }"
  ]

gate :: String -> Gate -> Arr String (Path -> DotGate)
     -> Path -> DotGate
gate nm Gate{..} env p =
  DotGate { inputNodes  = theInputs
          , outputNodes = theOutputs
          , blackbox    = theBlackbox
          , whitebox    = theWhitebox
          } where

  gateNode   = mkGate p nm
  theInputs  = map (mkNode p . inputName)  inputs
  theOutputs = map (mkNode p . outputName) outputs

  header str  = unlines $
    [ ""
    , "/***************************************************/"
    ] ++ str ++
    [ "/***************************************************/" ]

  theWhitebox = execWhiteBox $ do
    tell [ header [ "// The circuit's inputs and outputs" ] ]
    tell $ nodeCluster p "inputs" $ map (declareInput p) inputs
    tell $ nodeCluster p "outputs" $ map (declareOutput p) outputs

    tell [ "  node [shape = rectangle];" ]

    forM_ (reverse definitions) $ \ (os, e) -> do
      forM_ os $ \ o -> tell [ declareLocal p o ]
      case e of
        Alias x     -> case os of
          [y] -> tell [indent 2 $ mkArrow p x (outputName y) ]
          _   -> error "not yet supported"
        Call f args -> case findArr f env of
          Nothing   -> error "This should never happen"
          Just repr -> do
            id <- fresh
            let dotG = repr (extend id p)
            tell (blackbox dotG)
            forM_ (zip args (inputNodes dotG)) $ \ (arg, iport) ->
              tell [ arrow False (mkNode p (inputName arg)) iport ]
            forM_ (zip (outputNodes dotG) os) $ \ (oport, out) -> do
              let outName = mkNode p (outputName out)
              tell [ arrow (outName `elem` theOutputs) oport outName ]

    tell [ header [ "// Finally, we make sure the left-to-right ordering"
                  , "// of inputs and outputs is respected."
                  ]
         ]
    tell [ leftToRight "inputs" theInputs ]
    tell [ leftToRight "outputs" theOutputs ]

  theBlackbox = filter (/= "")
    [ "subgraph cluster_" ++ show p ++ " {"
    , "  style = invis;"
    , indent 2 $ gateNode ++ " [label = " ++ show nm ++ "];"
    , "  {"
    , "    node [shape = point, height = 0];"
    ,      unlines (map (indent 4 . declareInput p) inputs)
        ++ unlines (map (indent 4 . declareOutput p) outputs)
    , "  }"
    , leftToRight "inputs" theInputs
    , leftToRight "outputs" theOutputs
    , connectInputs theInputs gateNode
    , connectOutputs gateNode theOutputs
    , "}"
    ]

def :: Def -> Dot ()
def d = case toGate d of
  Nothing      -> pure ()
  Just (nm, g) -> do
    id <- freshId
    Dot $ modify $ \ s ->
      s { gates = insertArr (nm, gate nm g (gates s) . extend id) (gates s) }

dotDef :: Def -> Maybe DotGate
dotDef d = do
  st <- flip execStateT initDotSt $ runDot $ do
    -- we throw in some basic definitions so that whitebox has a change to succeed
    -- in practice we will only need 'nand'
    def notG
    def andG
    def d
  nm <- case d of { Stub{} -> Nothing; Def (nm, _) _ _ -> Just nm }
  ga <- findArr nm (gates st)
  pure (ga empty)

blackBoxDef :: Def -> [String]
blackBoxDef d = fromMaybe [] $ do
  ga <- dotDef d
  pure $
    [ "digraph blackbox {"
    , "  rankdir = TB;"
    , "  splines = ortho;"
    ]
    ++ blackbox ga
    ++ ["}"]

whiteBoxDef :: Def -> [String]
whiteBoxDef d = fromMaybe [] $ do
  ga <- dotDef d
  pure $
    [ "digraph whitebox {"
    , "  rankdir=TB;"
    , "  splines=ortho;"
    ]
    ++ whitebox ga
    ++ ["}"]



notG :: Def
notG = Def ("not", [PVar "x"]) [App "not" [Var "x"]] Nothing

andG :: Def
andG = Def ("and", [PVar "x", PVar "y"]) [Var "z"] $ Just
  [ [PVar "z"] :=: [App "and" [Var "x", Var "y"]]
  ]

swapG :: Def
swapG = Def ("swap", [PVar "x", PVar "y"]) [Var "y", Var "x"] Nothing

test :: IO ()
test = do
  let runner = putStrLn . unlines . whiteBoxDef
  runner notG
  runner swapG
  runner andG
  runner Anf.and4
  runner Anf.and4'
