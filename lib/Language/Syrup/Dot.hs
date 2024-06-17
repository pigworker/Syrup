------------------------------------------------------------------------------
-----                                                                    -----
-----     Dot: Compiler to Dot graph format                              -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Syrup.Dot where

import Control.Monad.Writer (MonadWriter, WriterT, tell, runWriterT)
import Control.Monad.State (StateT, evalStateT, execStateT, get, modify, put)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)
import Data.Foldable (for_)
import Data.Traversable (for)

import Language.Syrup.BigArray
import Language.Syrup.Syn
import Language.Syrup.Anf
import Language.Syrup.Smp
import Language.Syrup.Fsh
import Language.Syrup.Gph

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

  ins <- for is $ \ (Input _ i) -> do
     let node  = concat [gateNode, "__INPUTS:", i]
     let vnode = mkNode p i
     tellVirtual vnode
     tellEdge node vnode False
     pure node

  ous <- for os $ \ (Output _ o) -> do
     let node  = concat [gateNode, "__OUTPUTS:", o]
     let vnode = mkNode p o
     tellVirtual vnode
     tellEdge vnode node True
     pure node

  let iports = declarePorts 20 (inputName <$> is)
  let oports = declarePorts 20 (outputName <$> os)
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

  gph <- fmap concat $ for (reverse defs) $ \ (os, e) -> do
    for_ os $ tellVirtual . mkNode p . outputName -- TODO: use names of non-virtual vertices?
    case e of
      Alias x     -> case os of
        [y] -> [] <$ tellEdge (mkNode p x) (mkNode p $ outputName y) True
        _   -> error "not yet supported"
      Call f args -> case findArr f env of
        Nothing   -> error ("This should never happen: could not find " ++ f ++ ".")
        Just repr -> do
          id <- fresh
          let dotG = blackbox $ repr (extend id p)
          for_ (zip args (inputPorts dotG)) $ \ (arg, iport) ->
            tellEdge (mkNode p (inputName arg)) (iport ++ ":n") True
          for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
            tellEdge (oport ++ ":s") (mkNode p $ outputName out) False
          pure (circuitGraph dotG)
      FanIn args -> do
        id <- fresh
        let dotG = fanIn (extend id p) args os
        for_ (zip args (inputPorts dotG)) $ \ (arg, iport) ->
          tellEdge (mkNode p (inputName arg)) (iport ++ ":n") True
        for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
          tellEdge (oport ++ ":s") (mkNode p $ outputName out) False
        pure (circuitGraph dotG)
      FanOut arg -> do
        id <- fresh
        let dotG = fanOut (extend id p) [arg] os
        for_ (zip [arg] (inputPorts dotG)) $ \ (arg, iport) ->
          tellEdge (mkNode p (inputName arg)) (iport ++ ":n") True
        for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
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
  let gateNode   = "GATE_" ++ nm ++ "_" ++ show p
      iportNames = map (\ i -> gateNode ++ ":" ++ inputName i) is
      oportNames = map (\ o -> gateNode ++ ":" ++ outputName o) os
      iports     = declarePorts 7 (inputName <$> is)
      oports     = declarePorts 7 (outputName <$> os)
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

fanIn :: Path -> [Input] -> [Output] -> Circuit
fanIn p is os =
  let gateNode   = "FANIN_" ++ show p
      iportNames = map (\ i -> gateNode ++ ":" ++ inputName i) is
      iports     = declarePorts 7 (inputName <$> is)
  in Circuit iportNames [gateNode] $
    [ "subgraph fanin_" ++ show p ++ " {"
    , "  style = invis;"
    , indent 2 $ gateNode
    , "    [ shape = invtriangle"
    , "    , width = .75"
    , "    , height = .75"
    , "    , fixedsize = true"
    , "    , label = <<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"4\">"
    , indent 15 $ "<TR>" ++ unlines iports ++ "</TR>"
    , "              </TABLE>>"
    , "    ];"
    , "}"
    ]

fanOut :: Path -> [Input] -> [Output] -> Circuit
fanOut p is os =
  let gateNode   = "FANOUT_" ++ show p
      oportNames = map (\ o -> gateNode ++ ":" ++ outputName o) os
      oports     = declarePorts 7 (outputName <$> os)
  in Circuit [gateNode] oportNames $
    [ "subgraph fanout_" ++ show p ++ " {"
    , "  style = invis;"
    , indent 2 $ gateNode
    , "    [ shape = triangle"
    , "    , width = .75"
    , "    , height = .75"
    , "    , fixedsize = true"
    , "    , label = <<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"4\">"
    , indent 15 $ "<TR>" ++ unlines oports ++ "</TR>"
    , "              </TABLE>>"
    , "    ];"
    , "}"
    ]

declarePorts :: Int -> [String] -> [String]
declarePorts size = map declarePort where

  declarePort :: String -> String
  declarePort lb = concat
    [ "<TD PORT=", show lb, ">"
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

addDef :: DotSt -> Def -> DotSt
addDef st d = fromMaybe st $ flip execStateT st $ runDot $ def d

whiteBoxDef :: DotSt -> Def -> [String]
whiteBoxDef st d = fromMaybe [] $ do
  nm <- case d of { Stub{} -> Nothing; Def (nm, _) _ _ -> Just nm }
  ga <- findArr nm (gates st)
  pure $
    [ "digraph whitebox {"
    , "  rankdir = TB;"
    , "  nodesep = 0.2;"
    ]
    ++ circuitGraph (whitebox (ga empty))
    ++ ["}"]

myDotSt :: DotSt
myDotSt
  = foldl addDef initDotSt
  [ nand
  , dff
  ]
