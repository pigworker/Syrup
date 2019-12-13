------------------------------------------------------------------------------
-----                                                                    -----
-----     Gph: Representation of graphs                                  -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Syrup.SRC.Gph where

import Data.Foldable
import Syrup.SRC.BigArray

data Shape = Rectangle
  deriving (Eq, Show)

data Vertex
  = Visible   String (Maybe Shape) -- label
  | Invisible Bool                 -- splitting?
  deriving (Eq)

instance Semigroup Vertex where
  a <> b
    | a == b    = a
    | otherwise = error "This should never happen"

data Edge = Edge Bool -- directed?
  deriving (Eq)

instance Semigroup Edge where
  a <> b
    | a == b    = a
    | otherwise = error "This should never happen"

data Graph' v e = Graph
  { vertices :: Arr String v              -- named vertices
  , edges    :: Arr String (Arr String e) -- source -> targets
  }

type Graph = Graph' Vertex Edge

instance (Semigroup v, Semigroup e) => Semigroup (Graph' v e) where
  Graph vs es <> Graph ws fs = Graph (vs <> ws) (es <> fs)

instance (Semigroup v, Semigroup e) => Monoid (Graph' v e) where
  mempty = Graph emptyArr emptyArr

detectSplit :: Graph -> Graph
detectSplit (Graph vs es) = Graph vs' es where

  vs' = flip foldMapArr vs $ \ v@(str, vertex) ->
    case vertex of
      Visible{}   -> single v
      Invisible{} -> case findArr str es of
        Nothing                  -> single v
        Just ts | sizeArr ts > 1 -> single (str, Invisible True)
                | otherwise      -> single v

shrinkInvisible :: Graph -> Graph
shrinkInvisible g@(Graph vs es) = loop g es where

  loop g@(Graph vs es) queue = case popArr queue of
    Nothing                 -> g
    Just ((src, ts), queue) ->
      let (vs', es') = case foldMapArr pure ts of
            [(t, Edge False)] -> case (findArr t vs, findArr t es) of
              (Just (Invisible False), Just next) -> ( deleteArr t vs
                                                     , insertArr (src, next)
                                                     $ deleteArr src
                                                     $ deleteArr t es)
              _                                   -> (vs, es)
            _ -> (vs, es)
      in loop (Graph vs' es') queue

fromShape :: Maybe Shape -> String
fromShape = \case
  Nothing        -> "none"
  Just Rectangle -> "rectangle"

fromGraph :: Graph -> ([String], [String])
fromGraph Graph{..} =
  ( -- declare vertices first
    flip foldMapArr vertices $ \ (nm, v) -> pure $ case v of
      Visible lb sh -> nm ++ " [shape = " ++ fromShape sh ++ ", label =\"" ++ lb ++ "\"];"
      Invisible sp  -> nm ++ " [shape = point" ++ if sp then "];" else ", height = 0];"

  , -- then add edges
    flip foldMapArr edges $ \ (src, es) ->
      flip foldMapArr es $ \ (tgt, Edge dir) ->
        pure $ concat [ src
                      , " -> "
                      , tgt
                      , " [arrowsize = .5"
                      , if dir then "];" else " , dir = none];"
                      ]
  )
