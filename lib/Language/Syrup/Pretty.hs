------------------------------------------------------------------------------
-----                                                                    -----
-----     Pretty: Pretty printing for Syrup                              -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.Syrup.Pretty where

import Control.Monad.Reader (MonadReader, runReader)

import Data.Foldable (fold)
import Data.List (intercalate, intersperse)

import Prelude hiding (unwords, unlines)

import Language.Syrup.BigArray (emptyArr)
import Language.Syrup.Doc
import Language.Syrup.Syn
import Language.Syrup.Ty
import Language.Syrup.Unelab

------------------------------------------------------------------------
-- Resulting functions

prettyShow :: (Unelab s, Pretty (Unelabed s)) => CoEnv -> s -> Doc
prettyShow env = pretty . runUnelab env

basicShow :: (Unelab s, Pretty (Unelabed s)) => s -> Doc
basicShow = prettyShow emptyArr

csepShow :: (Unelab s, Pretty (Unelabed s)) => [s] -> Doc
csepShow = punctuate ", " . map basicShow

instance Pretty Va where
  prettyPrec _ = \case
    VQ    -> "?"
    V0    -> "0"
    V1    -> "1"
    VC vs -> pretty (AList vs)

------------------------------------------------------------------------
-- Pretty instances

data FunctionCall a = FunctionCall
  { functionName :: PrettyName
  , functionArgs :: [a]
  }

instance Pretty a => Pretty (FunctionCall a) where
  prettyPrec lvl = \case
    FunctionCall (RemarkableName IsZeroGate) [] -> "0"
    FunctionCall (RemarkableName IsOneGate) [] -> "1"
    FunctionCall (RemarkableName IsNotGate) [s] -> "!" <> prettyPrec NegatedClause s
    FunctionCall (RemarkableName IsOrGate) [s, t] ->
      parensIf (lvl > OrClause) $ unwords
        [ prettyPrec AndClause s
        , "|"
        , prettyPrec OrClause t
        ]
    FunctionCall (RemarkableName IsAndGate) [s, t] ->
      parensIf (lvl > AndClause) $ unwords
        [ prettyPrec NegatedClause s
        , "&"
        , prettyPrec AndClause t
        ]
    FunctionCall f es -> fold [annotate AnnFunction (pretty  (toName f)), pretty (ATuple es)]


instance Pretty (Exp' PrettyName ty) where
  prettyPrec lvl = \case
    Var _ x -> pretty x
    Hol _ x -> questionMark <> pretty x
    Cab _ es -> pretty (AList es)
    App _ f es -> prettyPrec lvl (FunctionCall f es)

instance Pretty a => Pretty (Pat' ty a) where
  prettyPrec lvl = \case
    PVar _ a -> pretty a
    PCab _ ps -> pretty (AList ps)

instance Pretty Ti where
  prettyPrec lvl = \case
    T0 -> "@"
    T1 -> ""

instance (Pretty t, Pretty x) => Pretty (Ty t x) where
  prettyPrec lvl = \case
    Meta x   -> annotate AnnType $ between "<?" ">" $ pretty x -- ugh...
    TVar s _ -> annotate AnnType $ between "<"  ">" $ pretty s
    Bit t    -> annotate AnnType $  pretty t <> text "<Bit>"
    Cable ps -> pretty (AList ps)

instance Pretty (Eqn' PrettyName ty) where
  prettyPrec _ (ps :=: es) =
    unwords
      [ csep $ map pretty ps
      , "="
      , csep $ map pretty es]

instance Pretty (Def' PrettyName Typ) where
  prettyPrec _ = \case
    Stub{} -> "Stubbed out definition"
    (Def (fn, ps) rhs meqns) ->
      -- Type declaration
      let pstys = map patTy ps in
      let lhsTy = pretty (FunctionCall fn pstys) in
      let rhstys = concatMap expTys rhs in
      let rhsTy = csep $ map pretty rhstys in
      let decl = unwords [lhsTy, "->", rhsTy] in
      -- Circuit definition
      let lhsDef = pretty (FunctionCall fn ps) in
      let rhsDef = csep $ map pretty rhs in
      let eqnDef = case meqns of
            Nothing -> []
            Just eqns -> annotate AnnKeyword "where"
              : map (nest 2 . pretty) eqns
      in
      let defn = unwords (lhsDef : "=" : rhsDef : eqnDef) in
      -- Combining everything
      unlines [decl, defn]

instance (Pretty t, Pretty x, Pretty t', Pretty x') => Pretty (TypeDecl' PrettyName t x t' x') where
  prettyPrec _ (TypeDecl fn is os) =
    let lhsTy = pretty (FunctionCall fn is) in
    let rhsTy = csep $ map pretty os in
    unwords [lhsTy, "->", rhsTy]
