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
import Data.List (intercalate, intersperse)
import Data.Void (Void, absurd)

import Prelude hiding (unwords, unlines)

import Language.Syrup.BigArray (emptyArr)
import Language.Syrup.Syn
import Language.Syrup.Ty

------------------------------------------------------------------------
-- Doc type and basic combinators

newtype Doc = Doc { runDoc :: String } -- for now

indent :: Int -> Doc -> Doc
indent i d = Doc (replicate i ' ') <> d

(<+>) :: Doc -> Doc -> Doc
d <+> e = d <> Doc " " <> e

between :: Doc -> Doc -> (Doc -> Doc)
between left right middle = fold [left, middle, right]

curlies :: Doc -> Doc
curlies = between (Doc "{") (Doc "}")

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

set :: [Doc] -> Doc
set = curlies . csep

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

newtype AList a = AList [a]
newtype ATuple a = ATuple [a]
newtype ASet a = ASet [a]

class Pretty t where
  pretty :: MonadPretty m => t -> m Doc
  pretty = prettyPrec minBound

  prettyPrec :: MonadPretty m => PrecedenceLevel -> t -> m Doc

instance Pretty String where
  prettyPrec _ = pure . Doc

instance Pretty Integer where
  prettyPrec _ = pretty . show

instance Pretty Void where
  prettyPrec _ = absurd

instance Pretty () where
  prettyPrec _ _ = pretty "()"

instance Pretty Unit where
  prettyPrec _ _ = pretty ""

instance Pretty a => Pretty (AList a) where
  prettyPrec _ (AList xs) = list <$> traverse pretty xs

instance Pretty a => Pretty (ATuple a) where
  prettyPrec _ (ATuple xs) = tuple <$> traverse pretty xs

instance Pretty a => Pretty (ASet a) where
  prettyPrec _ (ASet xs) = set <$> traverse pretty xs

instance Pretty Va where
  prettyPrec _ = \case
    VQ    -> pretty "?"
    V0    -> pretty "0"
    V1    -> pretty "1"
    VC vs -> pretty (AList vs)

------------------------------------------------------------------------
-- Pretty instances

data FunctionCall a = FunctionCall
  { functionName :: String
  , functionArgs :: [a]
  }

defaultApp :: (MonadPretty m, Pretty a) => String -> [a] -> m Doc
defaultApp f es = do
  f <- pretty f
  args <- pretty (ATuple es)
  pure $ fold [f, args]

instance Pretty a => Pretty (FunctionCall a) where
  prettyPrec lvl = \case
    FunctionCall f [] -> isRemarkable f >>= \case
      Just IsZeroGate | f == "zero" -> pure $ Doc "0"
      Just IsOneGate | f == "one" -> pure $ Doc "1"
      _ -> defaultApp f ([] :: [Exp' ()])
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
    Cab _ es -> pretty (AList es)
    App _ f es -> prettyPrec lvl (FunctionCall f es)

instance Pretty a => Pretty (Pat' ty a) where
  prettyPrec lvl = \case
    PVar _ a -> pretty a
    PCab _ ps -> pretty (AList ps)

instance Pretty Ti where
  prettyPrec lvl = prettyPrec lvl . \case
    T0 -> "@"
    T1 -> ""

instance (Pretty t, Pretty x) => Pretty (Ty t x) where
  prettyPrec lvl = \case
    Meta x   -> between (Doc "<?") (Doc ">") <$> pretty x -- ugh...
    TVar s _ -> between (Doc "<") (Doc ">") <$> pretty s
    Bit t    -> (<>) <$> pretty t <*> pretty "<Bit>"
    Cable ps -> pretty (AList ps)

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

instance (Pretty t, Pretty x, Pretty t', Pretty x') => Pretty (TypeDecl t x t' x') where
  prettyPrec _ (TypeDecl fn is os) = do
    lhsTy <- pretty (FunctionCall fn is)
    rhsTy <- csep <$> traverse pretty os
    pure $ unwords [lhsTy, Doc "->", rhsTy]

prettyShow :: Pretty t => CoEnv -> t -> String
prettyShow env = runDoc . flip runReader env . pretty

basicShow :: Pretty t => t -> String
basicShow = prettyShow emptyArr

csepShow :: Pretty t => [t] -> String
csepShow = intercalate ", " . map basicShow
