------------------------------------------------------------------------------
-----                                                                    -----
-----     Gph: Representation of graphs                                  -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Syrup.Gph where

import Data.Maybe (fromMaybe)

import Language.Syrup.BigArray

data Shape = Rectangle | ThrownAway
  deriving (Eq, Show)

data Vertex
  = Visible String        -- label
            (Maybe Shape) -- shape
  | Invisible
  deriving (Eq)

instance Semigroup Vertex where
  a <> b
    | a == b    = a
    | otherwise = error "This should never happen"

data Edge = Edge Int Bool -- directed?
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

shrinkInvisible :: Graph -> Graph
shrinkInvisible g@(Graph vs es) = loop g es where

  loop g@(Graph vs es) queue = case popArr queue of
    Nothing                 -> g
    Just ((src, ts), queue) ->
      let (vs', es') = case foldMapArr pure ts of
            [(t, Edge size False)] -> case (findArr t vs, findArr t es) of
              (Just Invisible, Just next) -> ( deleteArr t vs
                                                     , insertArr (src, next)
                                                     $ deleteArr src
                                                     $ deleteArr t es)
              _                                   -> (vs, es)
            _ -> (vs, es)
      in loop (Graph vs' es') queue

markDead :: [String] -> Graph -> Graph
markDead ous g@(Graph vs es) = loop g vs where

  loop g@(Graph vs es) queue = case popArr queue of
    Nothing -> g
    Just ((nm, v@Invisible), queue)
      | nm `notElem` ous ->
      let vs' = case fromMaybe emptyArr $ findArr nm es of
           arr | null arr -> insertArr (nm, Visible "" (Just ThrownAway))
               $ deleteArr nm vs
           _ -> vs
      in loop (Graph vs' es) queue
    Just (_, queue) -> loop g queue

fromShape :: Maybe Shape -> String
fromShape = \case
  Nothing        -> "none"
  Just Rectangle -> "rectangle"
  Just ThrownAway -> "circle"

fromGraph :: Graph -> ([String], [String])
fromGraph Graph{..} =
  ( -- declare vertices first
    flip foldMapArr vertices $ \ (nm, v) -> pure $ case v of
      Visible lb sh -> concat
        [ nm
        , " [shape = ", fromShape sh
        , " , label = ", show lb
        , if sh == Just ThrownAway then ", height = 0.075" else ""
        , "];"
        ]
      Invisible -> nm ++ " [label=\"\", shape=point, height=0];"

  , -- then add edges
    flip foldMapArr edges $ \ (src, es) ->
      flip foldMapArr es $ \ (tgt, Edge size dir) ->
        pure $ concat [ src
                      , " -> "
                      , tgt
                      , " [label=", show " ", ", arrowsize = .5"
                      , " penwidth= ", show (2 * size)
                      , if dir then "];" else " , dir = none];"
                      ]
  )
