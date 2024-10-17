------------------------------------------------------------------------------
-----                                                                    -----
-----     Pretty: Pretty printing for Syrup                              -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Syrup.Pretty where

import Control.Monad.Reader (MonadReader, runReader)

import Data.Foldable (fold)
import Data.List (intersperse)

import Prelude hiding (unwords, unlines)

import Language.Syrup.Syn (Exp'(..), Pat'(..), Eqn'(..), Def'(..), patTy, expTys)
import Language.Syrup.Ty (Ty(..), TypedDef, CoEnv, Remarkable(..), isRemarkable)

------------------------------------------------------------------------
-- Doc type and basic combinators

newtype Doc = Doc { runDoc :: String } -- for now

indent :: Int -> Doc -> Doc
indent i d = Doc (replicate i ' ') <> d

(<+>) :: Doc -> Doc -> Doc
d <+> e = d <> Doc " " <> e

between :: Doc -> Doc -> (Doc -> Doc)
between left right middle = fold [left, middle, right]

square :: Doc -> Doc
square = between (Doc "[") (Doc "]")

parens :: Doc -> Doc
parens = between (Doc "(") (Doc ")")

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

sepBy :: Doc -> [Doc] -> Doc
sepBy d = fold . intersperse d

unwords :: [Doc] -> Doc
unwords = sepBy (Doc " ")

unlines :: [Doc] -> Doc
unlines = sepBy (Doc "\n")

csep :: [Doc] -> Doc
csep = sepBy (Doc ", ")

list :: [Doc] -> Doc
list = square . csep

tuple :: [Doc] -> Doc
tuple = parens . csep

questionMark :: Doc
questionMark = Doc "?"

instance Semigroup Doc where
  Doc e <> Doc f = Doc (e <> f)

instance Monoid Doc where
  mempty = Doc ""
  mappend = (<>)

------------------------------------------------------------------------
-- Pretty class

type MonadPretty m =
  (MonadReader CoEnv m)

data PrecedenceLevel
  = OrClause
  | AndClause
  | NegatedClause
  deriving (Eq, Ord, Enum, Bounded)

class Pretty t where
  pretty :: MonadPretty m => t -> m Doc
  pretty = prettyPrec minBound

  prettyPrec :: MonadPretty m => PrecedenceLevel -> t -> m Doc

instance Pretty String where
  prettyPrec _ = pure . Doc

instance Pretty Integer where
  prettyPrec _ = pretty . show

------------------------------------------------------------------------
-- Pretty instances

data FunctionCall a = FunctionCall
  { functionName :: String
  , functionArgs :: [a]
  }

defaultApp :: (MonadPretty m, Pretty a) => String -> [a] -> m Doc
defaultApp f es = do
  f <- pretty f
  es <- traverse pretty es
  pure $ fold [f, tuple es]

instance Pretty a => Pretty (FunctionCall a) where
  prettyPrec lvl = \case
    FunctionCall f [s] -> isRemarkable f >>= \case
      Just IsNotGate | f == "not" -> (Doc "!" <>) <$> prettyPrec NegatedClause s
      _ -> defaultApp f [s]
    FunctionCall f [s, t] -> isRemarkable f >>= \case
      Just IsOrGate | f == "or" -> do
        s <- prettyPrec AndClause s
        t <- prettyPrec OrClause t
        pure $ parensIf (lvl > OrClause) $ unwords [s, Doc "|", t]
      Just IsAndGate | f == "and" -> do
        s <- prettyPrec NegatedClause s
        t <- prettyPrec AndClause t
        pure $ parensIf (lvl > AndClause) $ unwords [s, Doc "&", t]
      _ -> defaultApp f [s, t]
    FunctionCall f es -> defaultApp f es

instance Pretty (Exp' ty) where
  prettyPrec lvl = \case
    Var _ x -> pretty x
    Hol _ x -> (questionMark <>) <$> pretty x
    Cab _ es -> list <$> traverse pretty es
    App _ f es -> prettyPrec lvl (FunctionCall f es)

instance Pretty a => Pretty (Pat' ty a) where
  prettyPrec lvl = \case
    PVar _ a -> pretty a
    PCab _ ps -> list <$> traverse pretty ps


instance Pretty x => Pretty (Ty t x) where
  prettyPrec lvl = \case
    TyV x -> between (Doc "<") (Doc ">") <$> pretty x
    Bit _ -> pure $ Doc "<Bit>"
    Cable ps -> list <$> traverse pretty ps

instance Pretty (Eqn' ty) where
  prettyPrec _ (ps :=: es) = do
    ps <- csep <$> traverse pretty ps
    es <- csep <$> traverse pretty es
    pure $ unwords [ps, Doc "=", es]

instance Pretty TypedDef where
  prettyPrec _ = \case
    Stub{} -> pure (Doc "Stubbed out definition")
    (Def (fn, ps) rhs meqns) -> do
      -- Type declaration
      let pstys = map patTy ps
      lhsTy <- pretty (FunctionCall fn pstys)
      let rhstys = map (head . expTys) rhs
      rhsTy <- csep <$> traverse pretty rhstys
      let decl = unwords [lhsTy, Doc "->", rhsTy]
      -- Circuit definition
      lhsDef <- pretty (FunctionCall fn ps)
      rhsDef <- csep <$> traverse pretty rhs
      eqnDef <- case meqns of
        Nothing -> pure []
        Just eqns -> do
          eqns <- traverse pretty eqns
          pure [unlines (Doc "where" : map (indent 2) eqns)]
      let defn = unwords (lhsDef : Doc "=" : rhsDef : eqnDef)
      -- Combining everything
      pure $ unlines [decl, defn]

prettyShow :: Pretty t => CoEnv -> t -> String
prettyShow env = runDoc . flip runReader env . pretty
