------------------------------------------------------------------------------
-----                                                                    -----
-----     Dot: Compiler to Dot graph format                              -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Syrup.Dot where

import Control.Applicative ((<|>))
import Control.Monad (guard, unless)
import Control.Monad.State (MonadState, State, StateT, evalState, runStateT, execState, get, modify, put)
import Control.Monad.Writer (MonadWriter, WriterT, tell, runWriterT)

import Data.Char (isAlphaNum)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (for)

import Language.Syrup.Anf
import Language.Syrup.BigArray
import Language.Syrup.Fdk (Feedback(..))
import Language.Syrup.Fsh
import Language.Syrup.Gph
import Language.Syrup.Smp (nand, dff, zero)
import Language.Syrup.Syn (Def'(..), Ty(..))
import Language.Syrup.Ty

data Circuit = Circuit
  { inputPorts   :: [String]
  , outputPorts  :: [String]
  , circuitGraph :: [String]
  }

data DotGate = DotGate
  { blackbox    :: Circuit
  , whitebox    :: (Seq Feedback, Maybe Circuit)
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

data GateLoc = TopLevel | Unfolded
type Gates = Arr String ([String] -> GateLoc -> Path -> DotGate)

data DotSt = DotSt
  { supply :: Int
  , gates  :: Gates
  }

initDotSt :: DotSt
initDotSt = DotSt
  { supply = 0
  , gates  = emptyArr
  }

newtype Dot a = Dot { runDot :: State DotSt a }
  deriving (Functor, Applicative, Monad)

evalDot :: Dot a -> a
evalDot = flip evalState initDotSt . runDot

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
  { runWhiteBox :: StateT (Seq Feedback) (WriterT Graph (Fresh Int)) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadFresh Int
           , MonadWriter Graph
           , MonadState (Seq Feedback)
           )

evalWhiteBox :: WhiteBox a -> ((a, Seq Feedback), Graph)
evalWhiteBox = evalFresh . runWriterT . flip runStateT mempty . runWhiteBox

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


toWhitebox :: String -> Gate
           -> Gates    -- description of existing gates
           -> [String] -- list of gates that should be transparent
           -> GateLoc  -- is it a toplevel circuit or an unfolded one?
           -> Path     -- how much renaming should we do for the ports?
           -> WhiteBox (Maybe Circuit)
toWhitebox nm (Gate is os defs) env transparent loc p = do
  let gateNode   = mkGate p nm

  ins <- for is $ \ (Input _ ty _ i) -> do
     let vnode = mkNode p i
     tellVirtual vnode
     case loc of
       TopLevel -> do
         let node  = concat [gateNode, "__INPUTS:", i]
         tellEdge ty node vnode False
         pure node
       Unfolded -> pure vnode

  ous <- for os $ \ (Output _ ty _ o) -> do
    let vnode = mkNode p o
    tellVirtual vnode
    case loc of
      TopLevel -> do
        let node  = concat [gateNode, "__OUTPUTS:", o]
        tellEdge ty vnode node True
        pure node
      Unfolded -> pure vnode

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

  gph <- for (reverse defs) $ \ (os, e) -> do
    for_ os $ tellVirtual . mkNode p . outputName -- TODO: use names of non-virtual vertices?
    case e of
      Alias ty x -> case os of
        [y] -> Just [] <$ tellEdge ty (mkNode p x) (mkNode p $ outputName y) True
        _   -> do
          modify (<> Seq.singleton (AnImpossibleError "invalid alias structure"))
          pure Nothing
      Copy ty arg -> do
        id <- fresh
        let dotG = fanOut aCopy (extend id p) [arg] os
        for_ (zip [arg] (inputPorts dotG)) $ \ (arg, iport) ->
          tellEdge (inputType arg) (mkNode p (inputName arg)) (iport ++ ":n") False
        for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
          tellEdge (outputType out) (oport ++ ":s") (mkNode p $ outputName out) False
        pure (Just $ circuitGraph dotG)
      Call tys f args -> case findArr f env of
        Nothing   -> do
          unless (f `elem` ["nand", "dff", "zero"]) $
            modify (<> Seq.singleton (AMissingImplementation f))
          pure Nothing
        Just repr -> do
          id <- fresh
          (b, dotG) <- do
            let gt = repr transparent Unfolded (extend id p)
            if f `notElem` transparent then pure (True, blackbox gt) else
              case whitebox gt of
                (fdk, Nothing) -> do modify (<> fdk)
                                     pure (True, blackbox gt)
                (fdk, Just wbox) -> pure (False, wbox)
          for_ (zip args (inputPorts dotG)) $ \ (arg, iport) ->
            tellEdge (inputType arg) (mkNode p (inputName arg)) (iport ++ ":n") b
          for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
            tellEdge (outputType out) (oport ++ ":s") (mkNode p $ outputName out) False
          pure (Just $ circuitGraph dotG)
      FanIn args -> do
        id <- fresh
        let dotG = fanIn (extend id p) args os
        for_ (zip args (inputPorts dotG)) $ \ (arg, iport) ->
          tellEdge (inputType arg) (mkNode p (inputName arg)) (iport ++ ":n") False
        for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
          tellEdge (outputType out) (oport ++ ":s") (mkNode p $ outputName out) False
        pure (Just $ circuitGraph dotG)
      FanOut arg -> do
        id <- fresh
        let dotG = fanOut aFanOut (extend id p) [arg] os
        for_ (zip [arg] (inputPorts dotG)) $ \ (arg, iport) ->
          tellEdge (inputType arg) (mkNode p (inputName arg)) (iport ++ ":n") False
        for_ (zip (outputPorts dotG) os) $ \ (oport, out) -> do
          tellEdge (outputType out) (oport ++ ":s") (mkNode p $ outputName out) False
        pure (Just $ circuitGraph dotG)

  pure $ do
    gph <- sequence gph
    pure $ Circuit
      { inputPorts   = ins
      , outputPorts  = ous
      , circuitGraph =
          (case loc of { TopLevel -> [iPorts]; Unfolded -> [] }) ++
          -- needed to get the outputs at the bottom in e.g. tff
          [ "subgraph cluster_circuit__" ++ gateNode ++ " {"
          ] ++
          (case loc of
             TopLevel -> [ "  style=invis;" ]
             Unfolded ->
               [ "  style=solid;"
               , "  label=<<table border=\"0\"><tr><td border=\"1\">" ++ nm ++ "</td></tr></table>>;"
               , "  labeljust=l;"
               ]
          ) ++ concat gph ++
          [ "}" ]
          ++ (case loc of { TopLevel -> [oPorts]; Unfolded -> [] })
      }

gate :: String
     -> Gate
     -> Gates
     -> [String] -> GateLoc -> Path -> DotGate
gate nm g@Gate{..} env transparent b p =
  DotGate
    { blackbox    = toBlackbox p inputs nm outputs
    , whitebox    = theWhitebox
    } where

  theWhitebox = case evalWhiteBox (toWhitebox nm g env transparent b p) of
    ((Just (Circuit ins ous gts), fdk), gph) ->
      let optimized = shrinkInvisible $ detectSplit gph
          (vertices, edges) = fromGraph optimized
      in (fdk,) $ Just $ Circuit ins ous $ concat
        [ vertices
        , gts
        , edges
        ]
    ((Nothing, fdk), gph) -> (fdk, Nothing)

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
      s { gates = insertArr (nm, \ tr b p -> gate nm g (gates s) tr b (extend id p)) (gates s) }

addDef :: DotSt -> TypedDef -> DotSt
addDef st d = flip execState st $ runDot $ def d

whiteBoxDef :: DotSt     -- state of the dot representations
            -> [String]  -- gates that should be transparent
            -> TypedDef  -- definition to render
            -> (Seq Feedback, Maybe [String])
whiteBoxDef st transparetn (Stub nm _) = (Seq.singleton (ACannotDisplayStub nm), Nothing)
whiteBoxDef st transparent (Def (nm, _) _ _) = case findArr nm (gates st) of
  Nothing -> (Seq.singleton (ACouldntFindCircuitDiagram nm), Nothing)
  Just ga -> case whitebox $ ga transparent TopLevel empty of
    (fdk, Nothing) -> (fdk, Nothing)
    (fdk, Just circuit) -> (fdk,) $ Just $
      [ "digraph whitebox {"
      , "  rankdir = TB;"
      , "  nodesep = 0.2;"
      ]
      ++ circuitGraph circuit
      ++ ["}"]

myDotSt :: DotSt
myDotSt
  = foldl addDef initDotSt
  [ nand
  , dff
  , zero
  ]
