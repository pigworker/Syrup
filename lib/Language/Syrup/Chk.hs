------------------------------------------------------------------------------
-----                                                                    -----
-----     Chk: TypeChecking Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Language.Syrup.Chk where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Reader (local, runReaderT)
import Control.Monad.State (execStateT, runStateT, get, gets, put)
import Control.Monad.Writer (runWriterT, runWriter, tell)

import Data.Bifunctor (bimap)
import Data.Char (isAlpha)
import Data.Forget (forget)
import Data.Foldable (traverse_, fold)
import Data.IMaybe (fromIJust)
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import Data.Monoid (Last(Last), First(..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Data.Void (Void, absurd)

import Language.Syrup.BigArray
import Language.Syrup.Bwd
import Language.Syrup.Expt
import Language.Syrup.Fdk
import Language.Syrup.Pretty (basicShow, csepShow)
import Language.Syrup.Syn
import Language.Syrup.Ty
import Language.Syrup.Va

import Utilities.Lens (hasLens, use, (%=))

------------------------------------------------------------------------------
-- Checking whether a component is remarkable
------------------------------------------------------------------------------

isBisimilar :: Compo -> Compo -> Bool
isBisimilar c d = case bisimReport c d of
  Report (Bisimilar{}) -> True
  _ -> False

checkRemarkable :: Compo -> Maybe Remarkable
checkRemarkable cmp
  | null (memTys cmp)
  , [o] <- oupTys cmp
  , Bit _ <- getOutputType o
  = do guard (isJust (traverse_ (isBit . getInputType) $ inpTys cmp))
       case inpTys cmp of
         []    -> IsZeroGate <$ guard (isBisimilar cmp zeroCompo)
         [i]   -> IsNotGate  <$ guard (isBisimilar cmp notCompo)
         [i,j] -> IsNandGate <$ guard (isBisimilar cmp nandCompo)
              <|> IsAndGate  <$ guard (isBisimilar cmp andCompo)
              <|> IsOrGate   <$ guard (isBisimilar cmp orCompo)
         _ -> Nothing
checkRemarkable _ = Nothing

maybeRemarkable :: String -> Maybe Remarkable -> [Feedback]
maybeRemarkable str Nothing = []
maybeRemarkable str (Just g) = [] -- [str ++ " is a remarkable gate (" ++ enunciate g ++ ")."]
  where enunciate = \case
          IsZeroGate -> "zero"
          IsNotGate -> "not"
          IsNandGate -> "nand"
          IsAndGate -> "and"
          IsOrGate -> "or"

------------------------------------------------------------------------------
-- how to invoke the typechecker
------------------------------------------------------------------------------

execOnCoEnv :: CoEnv -> (forall s m. MonadCompo s m => m a) -> CoEnv
execOnCoEnv env act = fst $ runWriter $ execStateT act env

-- | The first boolean is telling us whether to attempt detecting whether
-- the gate is remarkable. We should not do so for built in ones because
-- such detection relies on them being already available as Compos!
mkComponent' :: MonadCompo s m
             => Bool -> (DEC, String) -> Maybe (Def, String)
             -> m (Bool, Maybe TypedDef)
mkComponent' isrmk (dec, decSrc) mdef =
  case (cookDec dec, mdef) of
    (dec@(Dec (g, ss) ts), Just (def, defSrc)) -> do
      env <- use hasLens
      case runWriterT $ runStateT (runReaderT (guts dec def) (B0 :< TySOURCE decSrc defSrc))
             (tySt0 {coEnv = env}) of
        Left (TyFailure ctx e) -> do
          tell $ Seq.fromList
            [ ATypeError $ typeErrorReport (ctx, e)
            , AStubbedOut g
            ]
          hasLens %= insertArr (g, stubOut dec)
          pure (False, Nothing)
        Right ((((ps, rhs), (qs0, qs1), def), st), YesHasHoles) -> do
          hasLens %= insertArr (g, stubOut dec)
          tell $ Seq.fromList
            [ AFoundHoles g $ flip foldMapArr (allHoles def) $ \ (k, v) ->
                ["  ?" ++ k ++ " : " ++ maybe "?" basicShow (getFirst v)]
            , AStubbedOut g
            ]
          pure (False, Nothing)
        Right ((((ps, rhs), (qs0, qs1), def), st), NoHasNoHoles) ->
          let mI  = memIn st
              mO  = memOu st
              ((ta0, k1), tar) =
                glom ([], foldMap support mI) (sched st)
              ((ta1, k2), tat) =
                glom ([], foldMap support (mI ++ ps)) (sched st)
          in  case (tat, foldMap support qs0 `subSet` k1,
                         foldMap support (mO ++ qs1) `subSet` k2) of
                ([], True, True) -> do
                  let mems = concat $ reverse $ memTy st
                  let rmk = guard isrmk >> checkRemarkable gc
                      gc = Compo
                        { monick = g
                        , rmk = rmk
                        , defn = Just def
                        , memTys = mems
                        , inpTys = zipWith (InputWire . pure) ps ss
                        , oupTys = zipWith (mkOutputWire mems) rhs ts
                        , stage0 = plan (Plan mI ta0 qs0)
                        , stage1 = plan (Plan (mI ++ ps) ta1 (mO ++ qs1))
                        }
                  tell $ Seq.fromList (ACircuitDefined [g] : maybeRemarkable g rmk)
                  hasLens %= insertArr (g, gc)
                  pure (True, Just def)
                e -> do -- trace (show (sched st)) $
                  let sin = case e of
                        (_, False, _) ->
                          Stage0 (foldMapSet (yank (sched st))
                                  (diffSet (foldMap support qs0) k1))
                        (_, _, False) ->
                          Stage1 (foldMapSet (yank (sched st))
                                  (diffSet (foldMap support (mO ++ qs1)) k2))
                        _ -> Junk
                  hasLens %= insertArr (g, stubOut dec)
                  tell $ Seq.fromList
                    [ ATypeError $ typeErrorReport (B0 :< TySOURCE decSrc defSrc, sin)
                    , AStubbedOut g
                    ]
                  pure (False, Nothing)
    (dec@(Dec (g, ss) ts), Nothing) -> do
      tell $ Seq.singleton (AStubbedOut g)
      hasLens %= insertArr (g, stubOut dec)
      pure (False, Nothing)


mkComponent :: MonadCompo s m
            => (DEC, String) -> Maybe (Def, String)
            -> m (Bool, Maybe TypedDef)
mkComponent = mkComponent' False

guts :: TyMonad m => Dec -> Def -> m (([Pat], [Exp]), ([Pat], [Pat]), TypedDef)
guts (Dec (g, ss) ts) (Def (f, ps) es eqs)
  | f /= g = tyErr (DecDef f g)
  | otherwise = do
  let ss' = map fogTy ss
  let ts' = map fogTy ts
  typs <- local (:< TyINPUTS (forget ss') ps) $ decPats ss' ps
  (qs, tyes) <- local (:< TyOUTPUTS ts' es) $ chkExps (map (, Nothing) ts') es
  st <- get
  eqs <- traverse (traverse (\ eq -> local (:< TyEQN eq) $ chkEqn eq)) eqs
  (qs', (qs0, qs1)) <- fold <$> traverse stage ts
  solders qs' qs
  def <- normDef (Def (f, typs) tyes eqs)
  return ((ps, es), (qs0, qs1), def)
guts (Dec (g, ss) ts) (Stub f msg)
  | f /= g    = tyErr (DecDef f g)
  | otherwise = tyErr (Stubbed msg)

stubOut :: Dec -> Compo
stubOut (Dec (g, ss) ts) = Compo
  { monick = g
  , rmk = Nothing
  , defn = Nothing
  , memTys = []
  , inpTys = InputWire  Nothing <$> ss
  , oupTys = OutputWire Nothing <$> ts
  , stage0 = const (fmap stub ts0)
  , stage1 = const (fmap stub ts1)
  } where (ts0, ts1) = foldMap splitTy2 ts


decPats :: TyMonad m => [Ty1] -> [Pat] -> m [TypedPat]
decPats [] [] = return []
decPats [] ps = tyErr LongPats
decPats ss [] = tyErr ShortPats
decPats (s : ss) (p : ps) = (:) <$> decPat Nothing s p <*> decPats ss ps

decPat :: TyMonad m
       => Maybe Ty1   -- the folded type
       -> Ty1         -- its current unfolding
       -> Pat         -- the pattern to typecheck
       -> m TypedPat
decPat _ s (PVar () x) = PVar (forget s) x <$ defineWire (Just (forget s)) (Physical x)
decPat _ s@(Cable ss) (PCab () ps)
  | length ss == length ps = PCab (forget s) <$> decPats ss ps
  | otherwise = tyErr CableWidth
decPat _ (Bit _) (PCab _ _) = tyErr BitCable
decPat _ (Meta x) _ = absurd x
decPat mty ty@(TVar _ t) p = decPat (mty <|> Just ty) t p

------------------------------------------------------------------------------
-- where-equations
------------------------------------------------------------------------------

chkEqn :: TyMonad m => Eqn -> m TypedEqn
chkEqn eqn@(qs :=: es) = do
  tqs <- traverse defPat qs
  (ps, es) <- chkExps (map (fmap Just) tqs) es
  solders qs ps
  pure (map snd tqs :=: es)

defPat :: TyMonad m => Pat -> m (Typ, TypedPat)
defPat (PVar () x)  = do
  ty <- defineWire Nothing (Physical x)
  pure (ty, PVar ty x)
defPat (PCab () ps) = do
  (tys, pats) <- unzip <$> traverse defPat ps
  let ty = Cable tys
  pure (ty, PCab ty pats)

chkExps :: TyMonad m => [(Typ, Maybe TypedPat)] -> [Exp] -> m ([Pat], [TypedExp])
chkExps []  []       = return ([], [])
chkExps tqs []       = tyErr LongPats
chkExps tqs (e : es) = do
  (ps, tqs, e) <- local (:< TyEXP e (fst <$> tqs)) $ chkExp tqs e
  bimap (ps ++) (e :) <$> chkExps tqs es

solders :: TyMonad m => [Pat] -> [Pat] -> m ()
solders [] [] = return ()
solders (q : qs) (p : ps) = solder q p >> solders qs ps
solders _ _ = tyErr BUGSolderMismatch

solder :: TyMonad m => Pat -> Pat -> m ()
solder (PCab () qs) (PCab () ps) = solders qs ps
solder q p = schedule ([q] :<- (id, [p]))

------------------------------------------------------------------------------
-- expressions
------------------------------------------------------------------------------

memRenaming :: Maybe (Pat' ty String) -> OutputWire -> [(CellName, String)]
memRenaming p (OutputWire mop _) = maybe [] (uncurry go) ((,) <$> p <*> mop) where

  go :: Pat' ty String -> OPat -> [(CellName, String)]
  go (PVar _ p)  (PVar _ (cn , b)) = if b then [(CellName cn, p)] else []
  go (PCab _ ps) (PCab _ qs)       = concat $ zipWith go ps qs
  go _ _ = []

memRenamings :: [Maybe (Pat' ty String)] -> [OutputWire] -> [(CellName, String)]
memRenamings ps os = concat $ zipWith memRenaming ps os

renameMem :: [(CellName, String)] -> MemoryCell -> MemoryCell
renameMem rho (MemoryCell mc t) = MemoryCell (fmap CellName $ mc >>= flip lookup rho) t

chkExp :: TyMonad m
       => [(Typ, Maybe TypedPat)]
       -> Exp
       -> m ([Pat], [(Typ, Maybe TypedPat)], TypedExp)
chkExp ((t,_) : tqs) (Var () x) = do
  s <- useWire (Physical x)
  local (:< TyWIRE x s t) $ tyEq (s, t)
  return ([PVar () x], tqs, Var s x)
chkExp ((t,_) : tqs) (Hol () x) = do
  s <- defineWire (Just t) (Holey x)
  tell YesHasHoles
  return ([PVar () ('?':x)], tqs, Hol s x)
chkExp tqs e@(App _ fn es) = do
  env <- gets coEnv
  f <- case findArr fn env of
    Nothing -> tyErr (Don'tKnow fn)
    Just f  -> return f
  -- rename the memory cells brought into scope by f
  let (ts, qs) = unzip tqs
  let rho = memRenamings qs (oupTys f)
  memTys <- pure $ map (renameMem rho) (memTys f)

  let mTy = getCellType <$> memTys
  mIn <- for mTy $ \ ty -> do
    w <- wiF
    defineWire (Just (fogTy ty)) (Physical w)
    return (PVar () w)
  mOu <- for mTy $ \ ty -> do
    w <- wiF
    defineWire (Just (fogTy ty))(Physical w)
    return (PVar () w)
  st <- get
  put $ st { memTy = memTys : memTy st
           , memIn = memIn st ++ mIn
           , memOu = memOu st ++ mOu
           }
  let iTy = getInputType <$> inpTys f
  (ps, es) <- local (:< TyAPP f es) $ chkExps (map (\t -> (fogTy t, Nothing)) iTy) es
  let oTy = getOutputType <$> oupTys f
  (qs, (qs0, qs1)) <- fold <$> traverse stage oTy
  schedule (qs0 :<- (stage0 f, mIn))
  schedule ((mOu ++ qs1) :<- (stage1 f, mIn ++ ps))
  let oTy' = map fogTy oTy
  (qs,,App oTy' fn es) <$> yield oTy' tqs
chkExp ((TVar s t, q) : tqs) (Cab () es) =
  chkExp ((forget t, q) : tqs) (Cab () es)
chkExp (tq : tqs) (Cab () es) = do
  sqs <- case tq of
    (Cable ss, Just (PCab _ qs))
      | length ss == length qs  -> return (zipWith (\ s q -> (s, Just q)) ss qs)
      | otherwise               -> return (map (, Nothing) ss)
    (Cable ss, Just (PVar _ _)) -> return (map (, Nothing) ss) -- TODO:?
    (Cable ss, Nothing)         -> return (map (, Nothing) ss)
    (Bit _, _) -> tyErr BitCable
    (Meta x, _) -> do
      ss <- traverse (const tyF) es
      tyEq (Cable ss, Meta x)
      return (map (, Nothing) ss)
  (ps, es) <- local (:< TyCAB es (fst <$> sqs)) $ chkExps sqs es
  return ([PCab () ps], tqs, Cab (Cable (map fst sqs)) es)
chkExp [] _ = tyErr ShortPats

yield :: TyMonad m => [Typ] -> [(Typ, a)] -> m [(Typ, a)]
yield []       tqs = return tqs
yield (s : ss) []  = tyErr ShortPats
yield (s : ss) ((t , q) : tqs) = tyEq (s, t) >> yield ss tqs

stage :: TyMonad m => Ty2 -> m ([Pat], ([Pat], [Pat]))
stage (Meta x) = absurd x
stage (TVar _ t) = stage (forget t)
stage (Bit t) = do
  w <- wiF
  defineWire (Just (Bit Unit)) (Physical w)
  return ([PVar () w], case t of {T0 -> ([PVar () w], []); T1 -> ([], [PVar () w])})
stage (Cable ts) = do
  (qs, (qs0, qs1)) <- fold <$> traverse stage ts
  return ([PCab () qs], ([PCab () qs0], [PCab () qs1]))

------------------------------------------------------------------------------
-- from raw to cooked Syrup types
------------------------------------------------------------------------------

data Dec
  = Dec (String, [Ty1]) [Ty2]

cookDec :: DEC -> Dec
cookDec (DEC (f, is) os) =
  Dec (f, fmap (cookTY Unit id) is) (fmap (cookTY T1 (const T0)) os)

cookTY :: t -> (t -> t) -> TY -> Ty t Void
cookTY t old (TYVAR x ty) = TVar x (cookTY t old (fromIJust ty))
cookTY t old BIT          = Bit t
cookTY t old (OLD ty)     = cookTY (old t) old ty
cookTY t old (CABLE tys)  = Cable (fmap (cookTY t old) tys)

------------------------------------------------------------------------------
-- error reporting
------------------------------------------------------------------------------

typeErrorReport :: (Bwd TyClue, TyErr) -> [String]
typeErrorReport (cz, e) = concat
  [ preamble, [""]
  , problem e
  , context e cz
  ]
  where
    getSrc (TySOURCE dec def) = Last (Just (lines dec ++ lines def))
    getSrc _ = Last Nothing
    preamble = case foldMap getSrc cz of
      (Last (Just ls)) ->
        "I was trying to make sense of the following code:" : "" : ls
      _ ->
        ["I can't remember what you wrote."]
    problem CableWidth = ["I found a cable with the wrong width."]
    problem BitCable   = ["I found a cable connected to a bit wire."]
    problem CableLoop  = ["I couldn't fit a cable inside itself."]
    problem (DecDef c f) = [ concat
      [ "I found a declaration for ", c
      , " but its definition was for ", f, "."] ]
    problem (Stubbed msg) = map (punctuate "\n" . render) msg
    problem (DuplicateWire x) = [ concat
      ["I found more than one signal called ", x, "."] ]
    problem LongPats  = ["I found fewer signals than I needed."]
    problem ShortPats = ["I found more signals than I needed."]
    problem (Don'tKnow f) = ["I didn't know what " ++ f ++ " was."]
    problem (Stage0 xs) = case foldMapSet human xs of
      [] -> [ "There were some signals I couldn't compute from memories."]
      xs -> [ "I couldn't compute the following just from memories:"
            , "  " ++ intercalate ", " xs
            , "Check definitions!"
            ]
    problem (Stage1 xs) = case foldMapSet human xs of
      [] -> [ "There were some signals I couldn't compute from inputs."]
      xs -> [ "I couldn't compute the following from inputs:"
            , "  " ++ intercalate ", " xs
            , "They're either stuck in a loop or missing entirely."
            ]
    problem Junk =
      ["There's some extra junk in this circuit" ++
       "that I can't see how to compute!"]
    problem BUGSolderMismatch = ["I messed up my internal wiring: report me!"]
    problem (ConflictingHoles x) = ["Conflicting uses for the hole name ?" ++ x ++ "."]

    context (Stubbed _) = const []
    context _ = ("" :) . context'
    context' (_ :< TyEQN eq) =
      ["At the time, I was checking this equation:", basicShow eq]
    context' (_ :< TyOUTPUTS ts es) =
      [ "I was trying to get outputs"
      , "  " ++ csepShow ts
      , "from expressions"
      , "  " ++ csepShow es
      , "at the time."
      ]
    context' (_ :< TyINPUTS ts ps) =
      [ "I was trying to fit inputs"
      , "  " ++ csepShow ts
      , "into the patterns"
      , "  " ++ csepShow ps
      , "at the time."
      ]
    context' (_ :< TyEXP e ts) =
      [ "I was hoping to get the beginning of these"
      , "  " ++ csepShow ts
      , "from the expression"
      , "  " ++ basicShow e
      , "at the time."
      ]
    context' (_ :< TyCAB es ts) =
      [ "I was trying to make a cable of these"
      , "  " ++ csepShow ts
      , "from the expressions"
      , "  " ++ csepShow es
      , "at the time."
      ]
    context' (_ :< TyAPP c es) =
      [ "I was trying to use " ++ monick c ++ " which wants input types"
      , "  " ++ csepShow (getInputType <$> inpTys c)
      , "but feeding in these expressions"
      , "  " ++ csepShow es
      , "at the time."
      ]
    context' (g :< TyWIRE x s t) =
      [ "I was trying to connect " ++ ww x
      , "but it carried a signal of type " ++ basicShow s
      , "where a signal of type " ++ basicShow t
      , "was expected."
      ] ++ context' g
      where
        ww (c : _) = "wire x"
        ww _       = "a wire"
    context' _ = ["I can't remember any more about what I thought I was doing."]

human :: String -> [String]
human s@(c : _) | isAlpha c = [s]
human _ = []

yank :: [Task] -> String -> Set String
yank _ x@(c : _) | isAlpha c = singleton x
yank ts x = foldMap go ts where
  go (qs :<- (_, ps))
    | x `inSet` foldMap support qs = foldMap support ps
    | otherwise = mempty


------------------------------------------------------------------------------
-- experiments
------------------------------------------------------------------------------

emptyCoEnv :: CoEnv
emptyCoEnv = emptyArr

nandCompo :: Compo
nandCompo = Compo
      { monick = "nand"
      , rmk = Just IsNandGate
      , defn = Nothing
      , memTys = []
      , inpTys = [ InputWire (Just (PVar () "X")) (Bit Unit)
                 , InputWire (Just (PVar () "Y")) (Bit Unit)
                 ]
      , oupTys = [ OutputWire Nothing (Bit T1) ]
      , stage0 = \ [] -> []
      , stage1 = \ i -> case i of
          [V0, _ ] -> [V1]
          [_ , V0] -> [V1]
          [V1, V1] -> [V0]
          _        -> [VQ]
      }

dffCompo :: Compo
dffCompo = Compo
      { monick = "dff"
      , rmk = Nothing
      , defn = Nothing
      , memTys = [MemoryCell (Just $ CellName "Q") (Bit Unit)]
      , inpTys = [InputWire  (Just (PVar () "D")) (Bit Unit)]
      , oupTys = [OutputWire (Just (PVar () ("Q", True))) (Bit T0)]
      , stage0 = \ [q] -> [q]
      , stage1 = \ [_, d] -> [d]
      }

zeroCompo :: Compo
zeroCompo = Compo
      { monick = "zero"
      , rmk = Just IsZeroGate
      , defn = Nothing
      , memTys = []
      , inpTys = []
      , oupTys = [OutputWire Nothing (Bit T0)]
      , stage0 = \ _ -> [V0]
      , stage1 = \ _ -> []
      }

myCoEnv :: CoEnv
myCoEnv = foldr insertArr emptyCoEnv
  [ ("nand", nandCompo)
  , ("dff", dffCompo)
  , ("zero", zeroCompo)
  ]

emptyTyEnv :: TyEnv
emptyTyEnv = emptyArr

myTyEnv :: TyEnv
myTyEnv = emptyTyEnv

env1, env2, env3, env4, env5, env6, env7, env8, env9 :: CoEnv
env1 = execOnCoEnv myCoEnv $ mkComponent
  (DEC ("not", [BIT]) [BIT], "!<Bit> -> <Bit>") $ Just
  (Def ("not", [PVar () "x"]) [Var () "y"] $ Just
   [[PVar () "y"] :=: [App [] "nand" [Var () "x", Var () "x"]]]
  ,"!x = y where  y = nand(x,x)")

notCompo :: Compo
notCompo = fromJust (findArr "not" env1)

env2 = execOnCoEnv env1 $ mkComponent
  (DEC ("and", [BIT, BIT]) [BIT], "<Bit> & <Bit> -> <Bit>") $ Just
  (Def ("and", [PVar () "x", PVar () "y"]) [Var () "b"] $ Just
    [[PVar () "a"] :=: [App [] "nand" [Var () "x", Var () "y"]]
    ,[PVar () "b"] :=: [App [] "not" [Var () "a"]]
    ]
  ,"x & y = b where  a = nand(x,y)  b = not(a)")

andCompo :: Compo
andCompo = fromJust (findArr "and" env2)

env3 = execOnCoEnv env2 $ mkComponent
  (DEC ("or", [BIT, BIT]) [BIT], "<Bit> | <Bit> -> <Bit>") $ Just
  (Def ("or", [PVar () "x", PVar () "y"]) [Var () "c"] $ Just
    [[PVar () "c"] :=: [App [] "nand" [Var () "a", Var () "b"]]
    ,[PVar () "a",PVar () "b"] :=: [App [] "not" [Var () "x"],App [] "not" [Var () "y"]]
    ]
  ,"x | y = c where  c = nand(a,b)  a,b = !x,!y")

orCompo :: Compo
orCompo = fromJust (findArr "or" env3)

env4 = execOnCoEnv env3 $ mkComponent
  (DEC ("jkff", [BIT, BIT]) [OLD BIT], "jkff(<Bit>,<Bit>) -> @<Bit>") $ Just
  (Def ("jkff", [PVar () "j", PVar () "k"]) [Var () "q"] $ Just
    [[PVar () "q"] :=: [App [] "dff" [Var () "d"]]
    ,[PVar () "d"] :=: [App [] "or"
       [  App [] "and" [Var () "j", App [] "not" [Var () "q"]]
       ,  App [] "and" [Var () "q", App [] "not" [Var () "k"]]
       ]]
    ]
  ,"jkff(j,k) = q where  q = dff(d)  d = j & !q | q & !k")

env5 = execOnCoEnv env4 $ mkComponent
  (DEC ("ndnff", [BIT]) [OLD BIT], "ndnff(<Bit>) -> @<Bit>") $ Just
  (Def ("ndnff", [PVar () "d"]) [App [] "not" [Var () "q"]] $ Just
    [[PVar () "q"] :=: [App [] "dff" [App [] "not" [Var () "d"]]]
    ]
  ,"ndnff(d) = !q where  q = dff(!d)")

env6 = execOnCoEnv env5 $ mkComponent
  (DEC ("xor", [BIT,BIT]) [BIT], "xor(<Bit>,<Bit>) -> <Bit>") $ Just
  (Def ("xor", [PVar () "x", PVar () "y"])
       [App [] "or" [ App [] "and" [App [] "not" [Var () "x"], Var () "y"]
                 , App [] "and" [Var () "x", App [] "not" [Var () "y"]]
                 ]]
       Nothing
  ,"xor(x,y) = !x & y | x & !y")

env7 = execOnCoEnv env6 $ mkComponent
  (DEC ("tff", [BIT]) [OLD BIT], "tff(<Bit>) -> @<Bit>") $ Just
  (Def ("tff", [PVar () "t"]) [Var () "q"] $ Just
    [[PVar () "q"] :=: [App [] "dff" [Var () "d"]]
    ,[PVar () "d"] :=: [App [] "xor" [Var () "t", Var () "q"]]
    ]
  ,"tff(t) = q where q = dff(d) d = xor(t,q)")

env8 = execOnCoEnv env7 $ mkComponent
  (DEC ("one", []) [OLD BIT], "one() -> @<Bit>") $ Just
  (Def ("one", []) [App [] "not" [App [] "zero" []]] Nothing
  ,"one() = !zero()")

env9 = execOnCoEnv env8 $ mkComponent
  (DEC ("tff2", [BIT]) [OLD BIT], "tff2(<Bit>) -> @<Bit>") $ Just
  (Def ("tff2", [PVar () "t"]) [App [] "xor" [Var () "q2",Var () "q1"]] $ Just
    [[PVar () "q2"] :=: [App [] "tff" [App [] "or" [App [] "not" [Var () "t"],Var () "q1"]]]
    ,[PVar () "q1"] :=: [App [] "tff" [App [] "one" []]]
    ]
  ,"tff2(T) = xor(Q2,Q1) where Q2 = tff(!T | Q1) Q1 = tff(one())")
