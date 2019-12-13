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
import Data.Char

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

data Circuit = Circuit
  { inputPorts   :: [String]
  , outputPorts  :: [String]
  , circuitGraph :: [String]
  }

data DotGate = DotGate
  { blackbox    :: Circuit
  , whitebox    :: Circuit
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

cleanupName :: String -> String
cleanupName = map cleanup where

  cleanup :: Char -> Char
  cleanup c | isAlphaNum c = c
            | otherwise    = '_'

mkNode :: Path -> String -> String
mkNode p str = "NODE_" ++ cleanupName str ++ "_" ++ show p

mkGate :: Path -> String -> String
mkGate p str = "GATE_" ++ cleanupName str ++ "_" ++ show p

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
         -> Path -> WhiteBox Circuit
toWhitebox nm (Gate is os defs) env p = do
  let gateNode   = mkGate p nm

  ins <- forM is $ \ (Input i) -> do
     let node  = concat [gateNode, "__INPUTS:", i]
     let vnode = mkNode p i
     tellVirtual vnode
     tellEdge node vnode False
     pure node

  ous <- forM os $ \ (Output _ o) -> do
     let node  = concat [gateNode, "__OUTPUTS:", o]
     let vnode = mkNode p o
     tellVirtual vnode
     tellEdge vnode node True
     pure node

  let (iports, oports) = declarePorts 20 is os
  let iPorts = unlines
         [ concat [ gateNode, "__INPUTS" ]
         , "    [ shape = none"
         , "    , label = <<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"10\">"
         , "               <TR>"
         , unlines $ map (indent 15) iports
         , "               </TR>"
         , "               </TABLE>>"
         , "      ];"
         ]

  let oPorts = unlines
         [ concat [ gateNode, "__OUTPUTS" ]
         , "    [ shape = none"
         , "    , label = <<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"10\">"
         , "               <TR>"
         , unlines $ map (indent 15) oports
         , "               </TR>"
         , "               </TABLE>>"
         , "      ];"
         ]

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
          let dotG = blackbox $ repr (extend id p)
          forM_ (zip args (inputPorts dotG)) $ \ (arg, iport) ->
            tellEdge (mkNode p (inputName arg)) (iport ++ ":n") True
          forM_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
            tellEdge (oport ++ ":s") (mkNode p $ outputName out) False
          pure (circuitGraph dotG)

  pure $ Circuit { inputPorts   = ins
                 , outputPorts  = ous
                 , circuitGraph = [ iPorts
                                  -- needed to get the outputs at the bottom in e.g. tff
                                  , "subgraph cluster_circuit__ {"
                                  , "  style=invis;"
                                  ]
                                  ++ gph ++
                                  ["}"
                                  , oPorts
                                  ]
                 }

gate :: String -> Gate -> Arr String (Path -> DotGate)
     -> Path -> DotGate
gate nm g@Gate{..} env p =
  DotGate { blackbox    = toBlackbox p inputs nm outputs
          , whitebox    = theWhitebox
          } where

  gateNode   = mkGate p nm

  theWhitebox =
    let (Circuit ins ous gts, gph) = evalWhiteBox (toWhitebox nm g env p)
        optimized                  = shrinkInvisible $ detectSplit gph
        (vertices, edges)          = fromGraph optimized
     in Circuit ins ous $ concat
     [ vertices
     , gts
     , edges
     ]

toBlackbox :: Path -> [Input] -> String -> [Output] -> Circuit
toBlackbox p is nm os =
  let gateNode         = "GATE_" ++ nm ++ "_" ++ show p
      iportNames       = map (\ i -> gateNode ++ ":" ++ inputName i) is
      oportNames       = map (\ o -> gateNode ++ ":" ++ outputName o) os
      (iports, oports) = declarePorts 7 is os
  in Circuit iportNames oportNames $
    [ "subgraph gate_" ++ show p ++ " {"
    , "  style = invis;"
    , indent 2 $ gateNode
    , "    [ shape = none"
    , "    , label = <<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"4\">"
    , unlines $ map (\ s -> indent 15 $ "<TR>" ++ s ++ "</TR>")
        [ unlines iports
        , concat [ "<TD COLSPAN=\"", show (max (length iports) (length oports)), "\">"
                 , "<FONT POINT-SIZE=\"20\">", nm, "</FONT>"
                 , "</TD>"
                 ]
        , unlines oports
        ]
    , "              </TABLE>>"
    , "    ];"
    , "}"
    ]

declarePorts :: Int -> [Input] -> [Output] -> ([String], [String])
declarePorts size is os =
  ( map (\ i -> declarePort (inputName i))  is
  , map (\ (Output _ nm) -> declarePort nm) os
  ) where

  declarePort :: String -> String
  declarePort lb = concat
    [ "<TD PORT=\"", lb, "\">"
    , if head lb == '_'
      then ""
      else concat ["<FONT POINT-SIZE=", show (show size), ">", lb, "</FONT>" ]
    , "</TD>"
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
    def hadd
    def fadd
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
    ]
    ++ circuitGraph (blackbox ga)
    ++ ["}"]

whiteBoxDef :: Def -> [String]
whiteBoxDef d = fromMaybe [] $ do
  ga <- dotDef d
  pure $
    [ "digraph whitebox {"
    , "  rankdir = TB;"
    , "  nodesep = 0.2;"
    ]
    ++ circuitGraph (whitebox ga)
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
  runner "hadd"       hadd
  runner "fadd"       fadd
  runner "rca4"       rca4
