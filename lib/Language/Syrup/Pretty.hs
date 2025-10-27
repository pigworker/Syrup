------------------------------------------------------------------------------
-----                                                                    -----
-----     Pretty: Pretty printing for Syrup                              -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Language.Syrup.Pretty where

import Control.Monad.Reader (MonadReader, runReader)

import Data.Foldable (fold)
import Data.List (intercalate, intersperse)
import Data.Void (Void, absurd)

import Prelude hiding (unwords, unlines)

import Language.Syrup.BigArray (emptyArr)
import Language.Syrup.Syn
import Language.Syrup.Ty

import qualified Text.PrettyPrint.Annotated as Doc
import Text.PrettyPrint.Annotated
  (annotate, braces, brackets, hcat, hsep, nest, parens, punctuate, text, vcat)

------------------------------------------------------------------------
-- Doc type and basic combinators

data SyrupAnnotations
  = Keyword
  | Type
  | Function

type Doc = Doc.Doc SyrupAnnotations

between :: Doc -> Doc -> (Doc -> Doc)
between left right middle = fold [left, middle, right]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

unwords :: [Doc] -> Doc
unwords = hsep -- punctuate " "

unlines :: [Doc] -> Doc
unlines = vcat -- sepBy "\n"

csep :: [Doc] -> Doc
csep = hcat . punctuate ", "

list :: [Doc] -> Doc
list =  brackets . csep

tuple :: [Doc] -> Doc
tuple = parens . csep

set :: [Doc] -> Doc
set = braces . csep

questionMark :: Doc
questionMark = "?"

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
  prettyPrec _ = pure . text

instance Pretty Name where
  prettyPrec _ = pretty . getName

instance Pretty TyName where
  prettyPrec _ = pretty . getTyName

instance Pretty Integer where
  prettyPrec _ = pretty . show

instance Pretty Void where
  prettyPrec _ = absurd

instance Pretty () where
  prettyPrec _ _ = pure "()"

instance Pretty Unit where
  prettyPrec _ _ = pure ""

instance Pretty a => Pretty (AList a) where
  prettyPrec _ (AList xs) = list <$> traverse pretty xs

instance Pretty a => Pretty (ATuple a) where
  prettyPrec _ (ATuple xs) = tuple <$> traverse pretty xs

instance Pretty a => Pretty (ASet a) where
  prettyPrec _ (ASet xs) = set <$> traverse pretty xs

instance Pretty Va where
  prettyPrec _ = \case
    VQ    -> pure "?"
    V0    -> pure "0"
    V1    -> pure "1"
    VC vs -> pretty (AList vs)

------------------------------------------------------------------------
-- Pretty instances

data FunctionCall a = FunctionCall
  { functionName :: Name
  , functionArgs :: [a]
  }

defaultApp :: (MonadPretty m, Pretty a) => Name -> [a] -> m Doc
defaultApp f es = do
  f <- pretty f
  args <- pretty (ATuple es)
  pure $ fold [annotate Function f, args]

instance Pretty a => Pretty (FunctionCall a) where
  prettyPrec lvl = \case
    FunctionCall f [] -> isRemarkable f >>= \case
      Just IsZeroGate | f == "zero" -> pure "0"
      Just IsOneGate | f == "one" -> pure "1"
      _ -> defaultApp f ([] :: [Exp' ()])
    FunctionCall f [s] -> isRemarkable f >>= \case
      Just IsNotGate | f == "not" -> ("!" <>) <$> prettyPrec NegatedClause s
      _ -> defaultApp f [s]
    FunctionCall f [s, t] -> isRemarkable f >>= \case
      Just IsOrGate | f == "or" -> do
        s <- prettyPrec AndClause s
        t <- prettyPrec OrClause t
        pure $ parensIf (lvl > OrClause) $ unwords [s, "|", t]
      Just IsAndGate | f == "and" -> do
        s <- prettyPrec NegatedClause s
        t <- prettyPrec AndClause t
        pure $ parensIf (lvl > AndClause) $ unwords [s, "&", t]
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
  prettyPrec lvl = pure . \case
    T0 -> annotate Keyword "@"
    T1 -> ""

instance (Pretty t, Pretty x) => Pretty (Ty t x) where
  prettyPrec lvl = \case
    Meta x   -> annotate Type . between "<?" ">" <$> pretty x -- ugh...
    TVar s _ -> annotate Type . between "<"  ">" <$> pretty s
    Bit t    -> (<> annotate Type (text "<Bit>")) <$> pretty t
    Cable ps -> pretty (AList ps)

instance Pretty (Eqn' ty) where
  prettyPrec _ (ps :=: es) = do
    ps <- csep <$> traverse pretty ps
    es <- csep <$> traverse pretty es
    pure $ unwords [ps, "=", es]

instance Pretty TypedDef where
  prettyPrec _ = \case
    Stub{} -> pure ("Stubbed out definition")
    (Def (fn, ps) rhs meqns) -> do
      -- Type declaration
      let pstys = map patTy ps
      lhsTy <- pretty (FunctionCall fn pstys)
      let rhstys = map (head . expTys) rhs
      rhsTy <- csep <$> traverse pretty rhstys
      let decl = unwords [lhsTy, "->", rhsTy]
      -- Circuit definition
      lhsDef <- pretty (FunctionCall fn ps)
      rhsDef <- csep <$> traverse pretty rhs
      eqnDef <- case meqns of
        Nothing -> pure []
        Just eqns -> do
          eqns <- traverse pretty eqns
          pure [unlines ("where" : map (nest 2) eqns)]
      let defn = unwords (lhsDef : "=" : rhsDef : eqnDef)
      -- Combining everything
      pure $ unlines [decl, defn]

instance (Pretty t, Pretty x, Pretty t', Pretty x') => Pretty (TypeDecl t x t' x') where
  prettyPrec _ (TypeDecl fn is os) = do
    lhsTy <- pretty (FunctionCall fn is)
    rhsTy <- csep <$> traverse pretty os
    pure $ unwords [lhsTy, "->", rhsTy]

{-
prettyShow :: Pretty t => CoEnv -> t -> String
prettyShow env = runDoc . flip runReader env . pretty

basicShow :: Pretty t => t -> String
basicShow = prettyShow emptyArr

csepShow :: Pretty t => [t] -> String
csepShow = intercalate ", " . map basicShow
-}
