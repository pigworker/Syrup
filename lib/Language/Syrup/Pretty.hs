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
import Language.Syrup.Utils (($$))

------------------------------------------------------------------------
-- Resulting functions

prettyUnelabed
  :: (Unelab s, Pretty (Unelabed s))
  => CoEnv -> s -> PrettyDoc (Unelabed s)
prettyUnelabed env = pretty . runUnelab env

prettyShow
  :: (Unelab s, Pretty (Unelabed s), Render (PrettyDoc (Unelabed s)))
  => CoEnv -> s -> String
prettyShow env = concat . renderToString . prettyUnelabed env

basicShow
  :: (Unelab s, Pretty (Unelabed s), Render (PrettyDoc (Unelabed s)))
  => s -> String
basicShow = prettyShow emptyArr

instance Pretty Va where
  type PrettyDoc Va = LineDoc
  prettyPrec _ = \case
    VQ    -> "?"
    V0    -> "0"
    V1    -> "1"
    VC vs -> brackets $$ map pretty vs

circuitExec :: Name -> CircuitConfig -> CircuitConfig -> LineDoc
circuitExec nm is os = fold
  [ pretty nm
  , let mems = memoryConfig is in
    if null mems then mempty else braces (foldMap pretty mems)
  , parens (foldMap pretty $ valuesConfig is)
  , " = "
  , let mems = memoryConfig os in
    if null mems then mempty else braces (foldMap pretty mems)
  , foldMap pretty (valuesConfig os)
  ]


------------------------------------------------------------------------
-- Pretty instances

instance Pretty PrettyName where
  type PrettyDoc PrettyName = LineDoc
  prettyPrec _ = pretty . toName

data FunctionCall a = FunctionCall
  { functionName :: PrettyName
  , functionArgs :: [a]
  }

instance (Pretty a, PrettyDoc a ~ LineDoc) => Pretty (FunctionCall a) where
  type PrettyDoc (FunctionCall a) = LineDoc
  prettyPrec lvl = \case
    FunctionCall (RemarkableName IsZeroGate) [] -> highlight AFunction "0"
    FunctionCall (RemarkableName IsOneGate) [] -> highlight AFunction "1"
    FunctionCall (RemarkableName IsNotGate) [s] ->
      highlight AFunction "!" <> prettyPrec NegatedClause s
    FunctionCall (RemarkableName IsOrGate) [s, t] ->
      parensIf (lvl > OrClause) $ unwords
        [ prettyPrec AndClause s
        , highlight AFunction "|"
        , prettyPrec OrClause t
        ]
    FunctionCall (RemarkableName IsAndGate) [s, t] ->
      parensIf (lvl > AndClause) $ unwords
        [ prettyPrec NegatedClause s
        , highlight AFunction "&"
        , prettyPrec AndClause t
        ]
    FunctionCall f es -> fold [pretty  (toName f), pretty (ATuple es)]


instance Pretty (Exp' PrettyName ty) where
  type PrettyDoc (Exp' PrettyName ty) = LineDoc
  prettyPrec lvl = \case
    Var _ x -> highlight AVariable $ pretty x
    Hol _ x -> "?" <> pretty x
    Cab _ es -> pretty (AList es)
    App _ f es -> prettyPrec lvl (FunctionCall f es)

instance (Pretty a, PrettyDoc a ~ LineDoc) => Pretty (Pat' ty a) where
  type PrettyDoc (Pat' ty a) = LineDoc
  prettyPrec lvl = \case
    PVar _ a -> highlight AVariable $ pretty a
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
    Stub{} -> aLine "Stubbed out definition"
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
