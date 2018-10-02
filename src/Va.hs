------------------------------------------------------------------------------
-----                                                                    -----
-----     Va: values for Syrup                                           -----
-----                                                                    -----
------------------------------------------------------------------------------

module Va where

import Data.List

import BigArray
import Syn


------------------------------------------------------------------------------
-- values
------------------------------------------------------------------------------

data Va
  = B0 | B1 | Bun [Va]
  deriving Eq

instance Show Va where
  show B0 = "0"
  show B1 = "1"
  show (Bun vs) = show vs


------------------------------------------------------------------------------
-- plans
------------------------------------------------------------------------------

data Plan = Plan [Pat] [Task] [Pat] deriving Show

plan :: Plan -> [Va] -> [Va]
plan (Plan ips tas ops) ivs = fmap (pval g') ops where
  g  = match ips ivs emptyArr
  g' = foldl task g tas


------------------------------------------------------------------------------
-- tasks
------------------------------------------------------------------------------

data Task = [Pat] :<- ([Va] -> [Va], [Pat])
instance Show Task where
  show (qs :<- (_, ps)) = show qs ++ " <- " ++ show ps

task :: Env -> Task -> Env
task g (qs :<- (f, ps)) = match qs (f (fmap (pval g) ps)) g


------------------------------------------------------------------------------
-- value environments
------------------------------------------------------------------------------

type Env = Arr String Va


------------------------------------------------------------------------------
-- matching
------------------------------------------------------------------------------

match :: [Pat] -> [Va] -> Env -> Env
match (PVar x : ps) (v : vs) =
  match ps vs . insertArr (x, v)
match (PCab ps : qs) (Bun vs : us) =
  match qs us . match ps vs
match _ _ = id


------------------------------------------------------------------------------
-- pval
------------------------------------------------------------------------------

pval :: Env -> Pat -> Va
pval g (PVar x) = case findArr x g of
  Nothing -> error "this isn't supposed to happen, you know"
  Just v  -> v
pval g (PCab ps) = Bun (fmap (pval g) ps)


------------------------------------------------------------------------------
-- glom
------------------------------------------------------------------------------

glom :: ([Task], Set String)  -- tasks in order, their support
     -> [Task]                -- tasks unscheduled
     -> ( ([Task], Set String)  -- tasks now scheduled
        , [Task]                -- tasks now unscheduled
        )
glom (tao, known) tas
  | null ta1  = ((tao, known), tas)
  | otherwise = glom (tao ++ ta1, known <> known') tar
  where
    (ta1, tar) = partition
      (\ (_ :<- (_, ps)) -> foldMap support ps `subSet` known)
      tas
    known' = foldMap (\ (qs :<- _) -> foldMap support qs) ta1
