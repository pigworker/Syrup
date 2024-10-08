------------------------------------------------------------------------------
-----                                                                    -----
-----     Dot: Compiler to Dot graph format                              -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Syrup.Dot where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Writer (MonadWriter, WriterT, tell, runWriterT)
import Control.Monad.State (StateT, evalStateT, execStateT, get, modify, put)

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)
import Data.Foldable (for_)
import Data.Traversable (for)

import Language.Syrup.Anf
import Language.Syrup.BigArray
import Language.Syrup.Fsh
import Language.Syrup.Gph
import Language.Syrup.Smp
import Language.Syrup.Syn
import Language.Syrup.Ty

data Circuit = Circuit
  { inputPorts   :: [String]
  , outputPorts  :: [String]
  , circuitGraph :: [String]
  }

data DotGate = DotGate
  { blackbox    :: Circuit
  , whitebox    :: Circuit
  }

type Port = (Bool, Maybe String, String)

inputToPort :: Input -> Port
inputToPort (Input b ty dn n) = (b, dn, n)

outputToPort :: Output -> Port
outputToPort (Output b ty dn n) = (b, dn, n)

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

tellEdge :: Typ -> String -> String -> Bool -> WhiteBox ()
tellEdge ty x y dir = tell (Graph emptyArr (single (x, single (y, Edge (size ty) dir))))

  where
    size :: Typ -> Int
    size (Bit _) = 1
    size (Cable ss) = sum (size <$> ss)
    size (TyV _) = 1 -- should never happen


toWhitebox :: String -> Gate -> Arr String (Path -> DotGate)
         -> Path -> WhiteBox Circuit
toWhitebox nm (Gate is os defs) env p = do
  let gateNode   = mkGate p nm

  ins <- for is $ \ (Input _ ty _ i) -> do
     let node  = concat [gateNode, "__INPUTS:", i]
     let vnode = mkNode p i
     tellVirtual vnode
     tellEdge ty node vnode False
     pure node

  ous <- for os $ \ (Output _ ty _ o) -> do
     let node  = concat [gateNode, "__OUTPUTS:", o]
     let vnode = mkNode p o
     tellVirtual vnode
     tellEdge ty vnode node True
     pure node

  let iports = map (declarePort 20 True . inputToPort) is
  let oports = map (declarePort 20 True . outputToPort) os
  let iPorts = if null iports then "" else unlines
         [ indent 2 $ concat [ gateNode, "__INPUTS" ]
         , "    [ shape = none"
         , "    , label = <<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"10\">"
         , "               <TR>"
         , unlines $ map (indent 15) iports
         , "               </TR>"
         , "               </TABLE>>"
         , "    ];"
         ]

  let oPorts = unlines
         [ indent 2 $ concat [ gateNode, "__OUTPUTS" ]
         , "    [ shape = none"
         , "    , label = <<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"10\">"
         , "               <TR>"
         , unlines $ map (indent 15) oports
         , "               </TR>"
         , "               </TABLE>>"
         , "    ];"
         ]

  gph <- fmap concat $ for (reverse defs) $ \ (os, e) -> do
    for_ os $ tellVirtual . mkNode p . outputName -- TODO: use names of non-virtual vertices?
    case e of
      Alias ty x -> case os of
        [y] -> [] <$ tellEdge ty (mkNode p x) (mkNode p $ outputName y) True
        _   -> error "not yet supported"
      Copy ty arg -> do
        id <- fresh
        let dotG = fanOut aCopy (extend id p) [arg] os
        for_ (zip [arg] (inputPorts dotG)) $ \ (arg, iport) ->
          tellEdge (inputType arg) (mkNode p (inputName arg)) (iport ++ ":n") False
        for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
          tellEdge (outputType out) (oport ++ ":s") (mkNode p $ outputName out) False
        pure (circuitGraph dotG)
      Call tys f args -> case findArr f env of
        Nothing   -> error ("This should never happen: could not find " ++ f ++ ".")
        Just repr -> do
          id <- fresh
          let dotG = blackbox $ repr (extend id p)
          for_ (zip args (inputPorts dotG)) $ \ (arg, iport) ->
            tellEdge (inputType arg) (mkNode p (inputName arg)) (iport ++ ":n") True
          for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
            tellEdge (outputType out) (oport ++ ":s") (mkNode p $ outputName out) False
          pure (circuitGraph dotG)
      FanIn args -> do
        id <- fresh
        let dotG = fanIn (extend id p) args os
        for_ (zip args (inputPorts dotG)) $ \ (arg, iport) ->
          tellEdge (inputType arg) (mkNode p (inputName arg)) (iport ++ ":n") False
        for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
          tellEdge (outputType out) (oport ++ ":s") (mkNode p $ outputName out) False
        pure (circuitGraph dotG)
      FanOut arg -> do
        id <- fresh
        let dotG = fanOut aFanOut (extend id p) [arg] os
        for_ (zip [arg] (inputPorts dotG)) $ \ (arg, iport) ->
          tellEdge (inputType arg) (mkNode p (inputName arg)) (iport ++ ":n") False
        for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
          tellEdge (outputType out) (oport ++ ":s") (mkNode p $ outputName out) False
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
      iports     = map (declarePort 7 False . inputToPort) is
      oports     = map (declarePort 7 False . outputToPort) os
  in Circuit iportNames oportNames $
    [ "subgraph gate_" ++ show p ++ " {"
    , "  style = invis;"
    , indent 2 $ gateNode
    , "    [ shape = none"
    , "    , label = <<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"4\">"
    , unlines $ map (\ s -> indent 15 $ "<TR>" ++ s ++ "</TR>") $
        (if null iports then id else (unlines iports :))
        [ concat [ "<TD COLSPAN=\"", show (max (length iports) (length oports)), "\">"
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
  let width = length is
      gateNode   = "FANIN_" ++ show p
      iportNames = map (\ i -> gateNode ++ ":" ++ inputName i) is
      oportNames = map (\ o -> gateNode ++ ":" ++ outputName o) os
      iports     = map (declarePort 7 False . inputToPort . \ r -> r { isVirtualInput = True}) is
      oports     = map (declarePort' (Just width) 7 False . outputToPort . \ r -> r { isVirtualOutput = True}) os
  in Circuit iportNames oportNames $
    [ "subgraph fanin_" ++ show p ++ " {"
    , "  style = invis;"
    , indent 2 $ gateNode
    , "    [ shape = none"
    , "    , style = filled"
    , "    , fillcolor = red"
    , "    , fixedsize = true"
    , "    , width = " ++ show (0.07 * fromIntegral width :: Double)
    , "    , height = .1"
    , "    , label = <<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\">"
    , indent 15 $ "<TR>" ++ unlines iports ++ "</TR>"
    , indent 15 $ "<TR>" ++ unlines oports ++ "</TR>"
    , "              </TABLE>>"
    , "    ];"
    , "}"
    ]

data FanOutType = FanOutType
  { fanOutName   :: String
  , fanOutColour :: String
  }

aFanOut :: FanOutType
aFanOut = FanOutType "FANOUT" "red"

aCopy :: FanOutType
aCopy = FanOutType "COPY" "skyblue"


fanOut :: FanOutType -> Path -> [Input] -> [Output] -> Circuit
fanOut fot p is os =
  let width = length os
      gateNode   = (fanOutName fot) ++ "_" ++ show p
      iportNames = map (\ i -> gateNode ++ ":" ++ inputName i) is
      oportNames = map (\ o -> gateNode ++ ":" ++ outputName o) os
      iports     = map (declarePort' (Just width) 7 False . inputToPort . \ r -> r { isVirtualInput = True}) is
      oports     = map (declarePort 7 False . outputToPort . \ r -> r { isVirtualOutput = True}) os
  in Circuit iportNames oportNames $
    [ "subgraph fanout_" ++ show p ++ " {"
    , "  style = invis;"
    , indent 2 $ gateNode
    , "    [ shape = none"
    , "    , style = filled"
    , "    , fillcolor = " ++ fanOutColour fot
    , "    , fixedsize = true"
    , "    , width = " ++ show (0.07 * fromIntegral width :: Double)
    , "    , height = .1"
    , "    , label = <<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\">"
    , indent 15 $ "<TR>" ++ unlines iports ++ "</TR>"
    , indent 15 $ "<TR>" ++ unlines oports ++ "</TR>"
    , "              </TABLE>>"
    , "    ];"
    , "}"
    ]

declarePort' :: Maybe Int -> Int -> Bool -> Port -> String
declarePort' mb size b (isv, dn, n) = concat
    [ "<TD PORT=", show n, maybe "" (\ i -> " COLSPAN=" ++ show (show i)) mb, ">"
    , let mn = n <$ guard (not isv) <|> dn <* guard b in
      maybe "" (\ lb -> concat ["<FONT POINT-SIZE=", show (show size), ">", lb, "</FONT>" ]) mn
    , "</TD>"
    ]

declarePort :: Int -> Bool -> Port -> String
declarePort = declarePort' Nothing

def :: TypedDef -> Dot ()
def d = case toGate d of
  Nothing      -> pure ()
  Just (nm, g) -> do
    id <- freshId
    Dot $ modify $ \ s ->
      s { gates = insertArr (nm, gate nm g (gates s) . extend id) (gates s) }

addDef :: DotSt -> TypedDef -> DotSt
addDef st d = fromMaybe st $ flip execStateT st $ runDot $ def d

whiteBoxDef :: DotSt -> TypedDef -> [String]
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
  , zero
  ]
