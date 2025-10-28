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
import Language.Syrup.Unelab

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
-- Pretty infrastructure

data PrecedenceLevel
  = OrClause
  | AndClause
  | NegatedClause
  deriving (Eq, Ord, Enum, Bounded)

newtype AList a = AList [a]
newtype ATuple a = ATuple [a]
newtype ASet a = ASet [a]

class Pretty t where
  pretty :: t -> Doc
  pretty = prettyPrec minBound

  prettyPrec :: PrecedenceLevel -> t -> Doc

instance Pretty String where
  prettyPrec _ = Doc

instance Pretty Name where
  prettyPrec _ = pretty . getName

instance Pretty Integer where
  prettyPrec _ = pretty . show

instance Pretty Void where
  prettyPrec _ = absurd

instance Pretty () where
  prettyPrec _ _ = pretty "()"

instance Pretty Unit where
  prettyPrec _ _ = pretty ""

instance Pretty a => Pretty (AList a) where
  prettyPrec _ (AList xs) = list $ map pretty xs

instance Pretty a => Pretty (ATuple a) where
  prettyPrec _ (ATuple xs) = tuple $ map pretty xs

instance Pretty a => Pretty (ASet a) where
  prettyPrec _ (ASet xs) = set $ map pretty xs

instance Pretty Va where
  prettyPrec _ = \case
    VQ    -> pretty "?"
    V0    -> pretty "0"
    V1    -> pretty "1"
    VC vs -> pretty (AList vs)

------------------------------------------------------------------------
-- Pretty instances

data FunctionCall a = FunctionCall
  { functionName :: PrettyName
  , functionArgs :: [a]
  }

instance Pretty a => Pretty (FunctionCall a) where
  prettyPrec lvl = \case
    FunctionCall (RemarkableName IsZeroGate) [] -> Doc "0"
    FunctionCall (RemarkableName IsOneGate) [] -> Doc "1"
    FunctionCall (RemarkableName IsNotGate) [s] -> Doc "!" <> prettyPrec NegatedClause s
    FunctionCall (RemarkableName IsOrGate) [s, t] ->
      parensIf (lvl > OrClause) $ unwords
        [ prettyPrec AndClause s
        , Doc "|"
        , prettyPrec OrClause t
        ]
    FunctionCall (RemarkableName IsAndGate) [s, t] ->
      parensIf (lvl > AndClause) $ unwords
        [ prettyPrec NegatedClause s
        , Doc "&"
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
  prettyPrec lvl = prettyPrec lvl . \case
    T0 -> "@"
    T1 -> ""

instance (Pretty t, Pretty x) => Pretty (Ty t x) where
  prettyPrec lvl = \case
    Meta x   -> between (Doc "<?") (Doc ">") $ pretty x -- ugh...
    TVar s _ -> between (Doc "<") (Doc ">") $ pretty (getTyName s) -- makes sure we don't unfold!
    Bit t    -> pretty t <> pretty "<Bit>"
    Cable ps -> pretty (AList ps)

instance Pretty (Eqn' PrettyName ty) where
  prettyPrec _ (ps :=: es) =
    unwords
      [ csep $ map pretty ps
      , Doc "="
      , csep $ map pretty es]

instance Pretty (Def' PrettyName Typ) where
  prettyPrec _ = \case
    Stub{} -> Doc "Stubbed out definition"
    (Def (fn, ps) rhs meqns) ->
      -- Type declaration
      let pstys = map patTy ps in
      let lhsTy = pretty (FunctionCall fn pstys) in
      let rhstys = concatMap expTys rhs in
      let rhsTy = csep $ map pretty rhstys in
      let decl = unwords [lhsTy, Doc "->", rhsTy] in
      -- Circuit definition
      let lhsDef = pretty (FunctionCall fn ps) in
      let rhsDef = csep $ map pretty rhs in
      let eqnDef = case meqns of
            Nothing -> []
            Just eqns -> Doc "where"
              : map (indent 2 . pretty) eqns
      in
      let defn = unwords (lhsDef : Doc "=" : rhsDef : eqnDef) in
      -- Combining everything
      unlines [decl, defn]

instance (Pretty t, Pretty x, Pretty t', Pretty x') => Pretty (TypeDecl' PrettyName t x t' x') where
  prettyPrec _ (TypeDecl fn is os) =
    let lhsTy = pretty (FunctionCall fn is) in
    let rhsTy = csep $ map pretty os in
    unwords [lhsTy, Doc "->", rhsTy]
