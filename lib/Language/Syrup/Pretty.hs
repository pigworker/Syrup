------------------------------------------------------------------------------
-----                                                                    -----
-----     Pretty: Pretty printing for Syrup                              -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Pretty where

import Control.Monad.Reader (MonadReader, runReader)

import Data.Foldable (fold)
import Data.List (intercalate, intersperse)
import Data.Void (Void, absurd)

import Prelude hiding (unwords, unlines)

import Language.Syrup.BigArray (emptyArr)
import Language.Syrup.Syn
import Language.Syrup.Ty
import Language.Syrup.Unelab

------------------------------------------------------------------------
-- Doc type and basic combinators

------------------------------------------------------------------------
-- Pretty infrastructure

data PrecedenceLevel
  = OrClause
  | AndClause
  | NegatedClause
  deriving (Eq, Ord, Enum, Bounded)

newtype AList a = AList [a]
newtype ATuple a = ATuple [a]
newtype ASet a = ASet [a]

instance Unelab a => Unelab (AList a) where
  type Unelabed (AList a) = AList (Unelabed a)
  unelab (AList a) = AList <$> unelab a

class Pretty t where
  pretty :: t -> Doc
  pretty = prettyPrec minBound

  prettyPrec :: PrecedenceLevel -> t -> Doc

instance Pretty String where
  prettyPrec _ = text

instance Pretty Name where
  prettyPrec _ = pretty . getName

instance Pretty TyName where
  prettyPrec _ = pretty . getTyName

instance Pretty Integer where
  prettyPrec _ = pretty . show

instance Pretty Void where
  prettyPrec _ = absurd

instance Pretty () where
  prettyPrec _ _ = "()"

instance Pretty Unit where
  prettyPrec _ _ = ""

instance Pretty a => Pretty (AList a) where
  prettyPrec _ (AList xs) = list $ map pretty xs

instance Pretty a => Pretty (ATuple a) where
  prettyPrec _ (ATuple xs) = tuple $ map pretty xs

instance Pretty a => Pretty (ASet a) where
  prettyPrec _ (ASet xs) = set $ map pretty xs

instance Pretty Va where
  prettyPrec _ = \case
    VQ    -> pure "?"
    V0    -> pure "0"
    V1    -> pure "1"
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
    FunctionCall f es -> fold [pretty  (toName f), pretty (ATuple es)]


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
  prettyPrec lvl = pure . \case
    T0 -> annotate Keyword "@"
    T1 -> ""

instance (Pretty t, Pretty x) => Pretty (Ty t x) where
  prettyPrec lvl = \case
    Meta x   -> annotate Type $ between "<?" ">" $ pretty x -- ugh...
    TVar s _ -> annotate Type $ between "<"  ">" $ pretty s
    Bit t    -> annotate Type $  pretty t <> text "<Bit>"
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
            Just eqns -> "where"
              : map (indent 2 . pretty) eqns
      in
      let defn = unwords (lhsDef : "=" : rhsDef : eqnDef) in
      -- Combining everything
      unlines [decl, defn]

instance (Pretty t, Pretty x, Pretty t', Pretty x') => Pretty (TypeDecl' PrettyName t x t' x') where
  prettyPrec _ (TypeDecl fn is os) =
    let lhsTy = pretty (FunctionCall fn is) in
    let rhsTy = csep $ map pretty os in
    unwords [lhsTy, "->", rhsTy]

prettyShow :: (Unelab s, Pretty (Unelabed s)) => CoEnv -> s -> Doc
prettyShow env = pretty . runUnelab env

basicShow :: (Unelab s, Pretty (Unelabed s)) => s -> Doc
basicShow = prettyShow emptyArr

csepShow :: (Unelab s, Pretty (Unelabed s)) => [s] -> Doc
csepShow = intercalate ", " . map basicShow
