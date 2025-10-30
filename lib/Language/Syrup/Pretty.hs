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


prettyShow
  :: (Unelab s, Pretty (Unelabed s), Render (PrettyDoc (Unelabed s)))
  => CoEnv -> s -> String
prettyShow env = renderToString . pretty . runUnelab env

basicShow
  :: (Unelab s, Pretty (Unelabed s), Render (PrettyDoc (Unelabed s)))
  => s -> String
basicShow = prettyShow emptyArr

csepShow :: (Unelab s, Pretty (Unelabed s), Render (PrettyDoc (Unelabed s))) => [s] -> String
csepShow = intercalate ", " . map basicShow


instance Pretty Va where
  type PrettyDoc Va = LineDoc
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

instance (Pretty a, PrettyDoc a ~ LineDoc) => Pretty (FunctionCall a) where
  type PrettyDoc (FunctionCall a) = LineDoc
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
    FunctionCall f es -> fold [pretty  (toName f), pretty (ATuple es)]


instance Pretty (Exp' PrettyName ty) where
  type PrettyDoc (Exp' PrettyName ty) = LineDoc
  prettyPrec lvl = \case
    Var _ x -> pretty x
    Hol _ x -> "?" <> pretty x
    Cab _ es -> pretty (AList es)
    App _ f es -> prettyPrec lvl (FunctionCall f es)

instance (Pretty a, PrettyDoc a ~ LineDoc) => Pretty (Pat' ty a) where
  type PrettyDoc (Pat' ty a) = LineDoc
  prettyPrec lvl = \case
    PVar _ a -> pretty a
    PCab _ ps -> pretty (AList ps)

instance Pretty Ti where
  type PrettyDoc Ti = LineDoc
  prettyPrec lvl = \case
    T0 -> "@"
    T1 -> ""

instance
  ( Pretty t
  , PrettyDoc t ~ LineDoc
  , Pretty x
  , PrettyDoc x ~ LineDoc
  ) => Pretty (Ty t x) where
  type PrettyDoc (Ty t x) = LineDoc
  prettyPrec lvl = \case
    Meta x   -> highlight AType $ between "<" ">" $ "?" <> pretty x
    TVar s _ -> pretty s
    Bit t    -> pretty t <> pretty (TyName "Bit")
    Cable ps -> pretty (AList ps)

instance Pretty (Eqn' PrettyName ty) where
  type PrettyDoc (Eqn' PrettyName ty) = LineDoc
  prettyPrec _ (ps :=: es) =
    unwords
      [ csep $ map pretty ps
      , "="
      , csep $ map pretty es]

instance Pretty (Def' PrettyName Typ) where
  type PrettyDoc (Def' PrettyName Typ) = Doc
  prettyPrec _ = \case
    Stub{} -> pretty ("Stubbed out definition" :: LineDoc)
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
      let defn = case meqns of
            Nothing -> pretty (unwords [lhsDef, "=", rhsDef])
            Just eqns ->
              pretty (unwords [lhsDef, "=", rhsDef, highlight AKeyword "where"])
              <> nest 2 (foldMap prettyBlock eqns)
      -- Combining everything
      in pretty decl <> defn

instance
  ( Pretty t
  , PrettyDoc t ~ LineDoc
  , Pretty x
  , PrettyDoc x ~ LineDoc
  , Pretty t'
  , PrettyDoc t' ~ LineDoc
  , Pretty x'
  , PrettyDoc x' ~ LineDoc
  ) => Pretty (TypeDecl' PrettyName t x t' x') where
  type PrettyDoc (TypeDecl' PrettyName t x t' x') = LineDoc
  prettyPrec _ (TypeDecl fn is os) =
    let lhsTy = pretty (FunctionCall fn is) in
    let rhsTy = csep $ map pretty os in
    unwords [lhsTy, "->", rhsTy]
