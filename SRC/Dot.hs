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
  )
import qualified Syrup.SRC.Anf as Anf
import Syrup.SRC.Smp
import Syrup.SRC.Fsh
import Syrup.SRC.Gph

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
declareOutput p (Output b nm) =
  let name = mkNode p nm in
  name ++ " [label = " ++ (if b then show "" else show nm) ++ "];"

declareLocal :: Path -> Output -> String
declareLocal p Output{..} =
  let name = mkNode p outputName in
  name ++ " [shape = point, height = 0];"


-- The monad we use to generate the content of the whitebox implementation:
-- we need a name supply & we would rather use Writer to collect the lines
-- of code rather than having to assemble everything by hand.
newtype WhiteBox a = WhiteBox
  { runWhiteBox :: WriterT Graph (Fresh Int) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadWriter Graph
           , MonadFresh Int
           )

evalWhiteBox :: WhiteBox a -> (a, Graph)
evalWhiteBox = evalFresh . runWriterT . runWhiteBox

tellVirtual :: String -> WhiteBox ()
tellVirtual nm = tell (Graph (single (nm, Invisible False)) emptyArr)

tellVertex :: String -> String -> Maybe Shape -> WhiteBox ()
tellVertex nm lb sh = tell (Graph (single (nm, Visible lb sh)) emptyArr)

tellEdge :: String -> String -> Bool -> WhiteBox ()
tellEdge x y dir = tell (Graph emptyArr (single (x, single (y, Edge dir))))

toWhitebox :: String -> Gate -> Arr String (Path -> DotGate)
         -> Path -> WhiteBox ([String], [String], [String])
toWhitebox nm (Gate is os defs) env p = do
  let gateNode   = mkGate p nm

  ins <- forM is $ \ (Input i) -> do
     let node  = mkNode p ("#INPUT" ++ i)
     let vnode = mkNode p i
     tellVertex node i Nothing
     tellVirtual vnode
     tellEdge node vnode False
     pure node

  ous <- forM os $ \ (Output _ o) -> do
     let node  = mkNode p ("#OUTPUT" ++ o)
     let vnode = mkNode p o
     tellVertex node o Nothing
     tellVirtual vnode
     tellEdge vnode node True
     pure node

  gph <- fmap concat $ forM (reverse defs) $ \ (os, e) -> do
    forM_ os $ tellVirtual . mkNode p . outputName -- TODO: use names of non-virtual vertices?
    case e of
      Alias x     -> case os of
        [y] -> [] <$ tellEdge (mkNode p x) (mkNode p $ outputName y) True
        _   -> error "not yet supported"
      Call f args -> case findArr f env of
        Nothing   -> error "This should never happen"
        Just repr -> do
          id <- fresh
          let dotG = repr (extend id p)
          forM_ (zip args (inputNodes dotG)) $ \ (arg, iport) ->
            tellEdge (mkNode p (inputName arg)) iport False
          forM_ (zip (outputNodes dotG) os) $ \ (oport, out) -> do
            tellEdge oport (mkNode p $ outputName out) False
          pure (blackbox dotG)

  pure (ins, ous, gph)

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
gate nm g@Gate{..} env p =
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

  theWhitebox =
    let ((ins, ous, gts), gph) = evalWhiteBox (toWhitebox nm g env p)
        optimized              = shrinkInvisible $ detectSplit gph
        (vertices, edges)      = fromGraph optimized
     in concat
     [ nodeCluster p "inputs" ins
     , nodeCluster p "outputs" ous
     , vertices
     , gts
     , edges
     , [ leftToRight "inputs" ins, leftToRight "outputs" ous ]
     ]

  theBlackbox = filter (/= "")
    [ "subgraph cluster_" ++ show p ++ " {"
    , "  style = invis;"
    , indent 2 $ gateNode ++ " [shape = rectangle, label = " ++ show nm ++ "];"
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
    -- we throw in some basic definitions so that whitebox has a
    -- chance to succeed. In practice we will only need 'nand'
    -- and 'dff'.
    def nand
    def notG
    def andG
    def orG
    def dff
    def xor
    def mux
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

runner :: String -> Def -> IO ()
runner nm d = do
  let content = unlines $ whiteBoxDef d
  writeFile ("whitebox-" ++ nm ++ ".dot") content

test :: IO ()
test = do
  runner "swap"       swapG
  runner "and"        andG
  runner "and4-prime" and4'
  runner "mux"        mux2
  runner "not"        notG
  runner "and4"       and4
  runner "tff"        tff
  runner "xor"        xor
