------------------------------------------------------------------------------
-----                                                                    -----
-----     DNF: Disjunctive Normal Forms                                  -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Syrup.DNF where

import Control.Applicative (liftA2) -- for compatibility
import Control.Monad (guard, join)
import Control.Monad.Reader (MonadReader, runReader)

import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Syrup.Syn
import Language.Syrup.Ty

import Utilities.Bwd

------------------------------------------------------------------------
-- Reducing a circuit to Disjunctive Normal Form
------------------------------------------------------------------------

data Occurrence
  = Asserted
  | Negated
  deriving (Eq, Ord)

negOcc :: Occurrence -> Occurrence
negOcc Asserted = Negated
negOcc Negated = Asserted

applyOcc :: Occurrence -> DNF -> DNF
applyOcc Asserted e = e
applyOcc Negated e = nae e

mergeOcc :: Maybe Occurrence -> Maybe Occurrence -> Maybe Occurrence
mergeOcc Nothing b = b
mergeOcc a Nothing = a
mergeOcc (Just a) (Just b) = a <$ guard (a == b)

type AndClause = Map String Occurrence
newtype DNF = DNF { getDNF :: Set AndClause }
  deriving (Eq, Ord)

instance Show DNF where
  show x@(DNF e)
    | x == tt = "True"
    | x == ff = "False"
    | otherwise = unions (Set.toList e)

   where

     unions = intercalate " | " . map (prods . Map.toList)
     prods  = intercalate " & " . map prod
     prod (k, occ) = occurrence occ ++ k
     occurrence Asserted = ""
     occurrence Negated = "!"

atom :: String -> Occurrence -> DNF
atom k occ = DNF $ Set.singleton $ Map.singleton k occ

pos :: String -> DNF
pos k = atom k Asserted

neg :: String -> DNF
neg k = atom k Negated

ff :: DNF
ff = DNF Set.empty

tt :: DNF
tt = DNF $ Set.singleton Map.empty

(|||) :: DNF -> DNF -> DNF
x@(DNF e) ||| y@(DNF f)
  | x == tt = tt
  | y == tt = tt
  | otherwise = DNF (Set.union e f)

setMapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
setMapMaybe f as
  = Set.map fromJust
  $ Set.filter isJust
  $ Set.map f as

simpleAnd :: DNF -> DNF -> DNF
simpleAnd (DNF e) (DNF f) = DNF $
  let unions = Set.cartesianProduct e f in
  flip setMapMaybe unions $ \ (a, b) -> do
    let mprods = Map.unionWith mergeOcc (Just <$> a) (Just <$> b)
    let (absurd, prods) = Map.mapEither (maybe (Left ()) Right) mprods
    guard (null absurd)
    pure $ prods

(&&&) :: DNF -> DNF -> DNF
e &&& f = noIndependent (simpleAnd e f)

nae :: DNF -> DNF
nae (DNF e)
  = noIndependent
  $ Set.foldr simpleAnd tt
  $ Set.map (Map.foldr (|||) ff . Map.mapWithKey (\ k -> atom k . negOcc)) e

noIndependent :: DNF -> DNF
noIndependent x@(DNF e) = fixpoint False (Map.toList <$> Set.toList e)

  where

    -- We are assuming these are sorted, and entries are using distinct
    -- strings. Weare looking for something of the shape
    -- [X, Y, !Z, !A, S, T]
    -- [X, Y, !Z,  A, S, T]
    --            ^^^
    -- in which case the value of A is independent and we can return the
    -- simplified conjunction [X, Y, !Z, S, T]
    hasIndependent
      :: [(String, Occurrence)]
      -> [(String, Occurrence)]
      -> Maybe [(String, Occurrence)]
    hasIndependent [] [] = Nothing
    hasIndependent ((k, occk) : es) ((l, occl) : fs) = do
      guard (k == l)
      if occk == occl
        then ((k, occk) :) <$> hasIndependent es fs
        else es <$ guard (es == fs)
    hasIndependent _ _ = Nothing

    go :: [[(String, Occurrence)]] -> Either Bool [[(String, Occurrence)]]
    go [] = Left False
    go ([] : es) = Left True
    go (e : es)  = case findJust (hasIndependent e) es of
      Nothing  -> (e:) <$> go es
      Just (e', es') -> pure (e' : es')

    fixpoint b es = case go es of
      Right forms -> fixpoint True forms
      Left True -> tt
      Left False | b -> DNF (Set.fromList $ map Map.fromList es)
      _ -> x

    -- | Locates the first value that satisfies the predicate
    --   and removes it from the list
    findJust :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
    findJust p [] = Nothing
    findJust p (x : xs)
      | Just a <- p x = pure (a, xs)
      | otherwise = fmap (x:) <$> findJust p xs

fromDNF :: forall ty. AllRemarkables ty -> DNF -> Exp' ty
fromDNF d
  = orClauses
  . map andClauses
  . Set.toList
  . Set.map Map.toList
  . getDNF

  where
    ty :: ty
    ty = bitTypeName d

    orClauses :: [Exp' ty] -> Exp' ty
    orClauses [] = App [ty] (zeroGateName d) []
    orClauses [e] = e
    orClauses (e:es) = App [ty] (orGateName d) [e, orClauses es]

    notGate :: Occurrence -> Exp' ty -> Exp' ty
    notGate Asserted e = e
    notGate Negated e = App [ty] (notGateName d) [e]

    andClauses :: [(String, Occurrence)] -> Exp' ty
    andClauses [] = App [ty] (notGateName d) [App [ty] (zeroGateName d) []]
    andClauses [(k, occ)] = notGate occ (Var ty k)
    andClauses ((k, occ) : es)
      = App [ty] (andGateName d)
        [notGate occ (Var ty k), andClauses es]

eval :: (String -> Maybe DNF) -> DNF -> DNF
eval rho
  = Set.foldr (|||) ff
  . Set.map (flip Map.foldrWithKey tt $ \ k occ -> (&&&) $ case rho k of
                Nothing -> atom k occ
                Just dnf -> applyOcc occ dnf)
  . getDNF

test :: DNF
test = flip eval (nae (pos "X" &&& neg "Y" ||| pos "Z")) $ \case
  "X" -> pure (pos "S" ||| pos "T")
  "Z" -> pure (nae (pos "A" &&& neg "B"))
  _ -> Nothing

dnf :: CoEnv -> Def' ty -> Def' ty
dnf env d@(Def lhs rhs meqns) = fromMaybe d $ do
  (ty : _, rhs') <- fmap unzip $ sequence $ runReader (traverse toDNF rhs) env
  let sub' = maybe id eval $ toAssignment env =<< meqns
  d <- allRemarkables env ty
  let rhs'' = fromDNF d <$> (sub' <$> rhs')
  pure (Def lhs rhs'' Nothing)
dnf env d = d

toAssignment :: CoEnv -> [Eqn' ty] -> Maybe (String -> Maybe DNF)
toAssignment env eqns = do
  kvs <- flip traverse eqns $ \case
    ([PVar _ x] :=: [b]) -> pure (x, fmap snd $ runReader (toDNF b) env)
    _ -> Nothing
  pure (join . flip lookup kvs)

toDNF :: MonadReader CoEnv m => Exp' ty -> m (Maybe (ty, DNF))
toDNF (Var ty x) = pure (Just (ty, pos x))
toDNF Cab{} = pure Nothing
toDNF Hol{} = pure Nothing
toDNF (App [ty] fn [e]) = isRemarkable fn >>= \case
    Just IsNotGate -> do
      ce <- fmap snd <$> toDNF e
      pure $ (ty,) . nae <$> ce
    _ -> pure Nothing
toDNF (App [ty] fn [e,f]) = isRemarkable fn >>= \case
    Just IsOrGate  -> do
      ce <- fmap snd <$> toDNF e
      cf <- fmap snd <$> toDNF f
      pure $ (ty,) <$> liftA2 (|||) ce cf
    Just IsAndGate -> do
      ce <- fmap snd <$> toDNF e
      cf <- fmap snd <$> toDNF f
      pure $ (ty,) <$> liftA2 (&&&) ce cf
    _ -> pure Nothing
toDNF App{} = pure Nothing

------------------------------------------------------------------------
-- Producing a DNF circuit from a truth table
------------------------------------------------------------------------

-- The output of a truth table can be understood like
-- a decision tree
data Decision
  = Always Bool
  | Inspect String Decision Decision
  deriving Show

evaluate :: (String -> Bool) -- a meaning for variable names
         -> Decision         -- a decision tree
         -> Bool             -- the associated output
evaluate env = \case
  Always b -> b
  Inspect x false true
    | env x -> evaluate env true
    | otherwise -> evaluate env false

unriffle :: [a] -> Maybe ([a], [a])
unriffle [] = pure ([], [])
unriffle (l : r : xs) = bimap (l :) (r :) <$> unriffle xs
unriffle _ = Nothing

ttToDecision :: [String] -> [Bool] -> Maybe Decision
ttToDecision xs = go (Lin <>< xs) . map Always where

  go :: Bwd String -> [Decision]
     -> Maybe Decision
  go Lin       [d] = pure d
  go (sx :< x) ds  = do
    (dsxF, dsxT) <- unriffle ds
    go sx (zipWith (Inspect x) dsxF dsxT)
  go _ _ = Nothing

decisionToDNF :: Decision -> DNF
decisionToDNF (Always b) = if b then tt else ff
decisionToDNF (Inspect x dxF dxT)
  =     (neg x &&& decisionToDNF dxF)
        ||| (pos x &&& decisionToDNF dxT)

ttToDef :: CoEnv -> String -> [String] -> [Bool] -> Maybe TypedDef
ttToDef env f xs bs = do
  dnf <- decisionToDNF <$> ttToDecision xs bs
  let bit = Bit Unit
  rmk <- allRemarkables env bit
  let exp = fromDNF rmk dnf
  pure (Def (f, map (PVar bit) xs) [exp] Nothing)
