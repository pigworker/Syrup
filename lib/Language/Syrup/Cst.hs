------------------------------------------------------------------------------
-----                                                                    -----
-----     Cst: Costing for Syrup                                         -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Cst where

import Control.Monad.State

import Data.Monoid (Sum(..))

import Language.Syrup.BigArray
import Language.Syrup.Syn
import Language.Syrup.Ty

-- costing in terms of the specified gates (and those that do
-- not have a definition)
type Costing = Arr Name (Sum Int)

costing :: CoEnv             -- Environment containing definitions
        -> Set Name          -- Express costing in term of these
        -> Name              -- Definition to cost
        -> Costing           -- Final costing
costing env supp fn = evalState (loop fn) emptyArr where

  defaultCost :: Name -> State (Arr Name Costing) Costing
  defaultCost fn = do
    let cost = single (fn, Sum 1)
    modify (insertArr (fn, cost))
    pure cost

  loop :: Name -> State (Arr Name Costing) Costing
  loop fn = gets (findArr fn) >>= \case
    -- If the circuit has already been costed, return that
    Just cost -> pure cost
    -- If it cannot be found, then we need to cost it
    -- if it is part of the support then its cost is 1 of itself
    Nothing | fn `inSet` supp -> defaultCost fn
    -- Otherwise we break it into its components and cost them recursively
    Nothing | otherwise ->
      -- look its definition up
      case defn =<< findArr fn env of
        Just (d@Def{}) -> do
          costs <- flip travArr (allGates d) $ \ (comp, number) ->
                     do cost <- loop comp
                        pure ((number *) <$> cost)
          let cost = foldMapArr snd costs
          modify (insertArr (fn, cost))
          pure cost
        -- if it does not have a definition, its cost is 1 of itself
        _ -> defaultCost fn
