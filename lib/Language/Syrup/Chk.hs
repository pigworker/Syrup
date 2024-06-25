------------------------------------------------------------------------------
-----                                                                    -----
-----     Chk: TypeChecking Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Chk where


import Control.Monad.Reader (local)
import Control.Monad.State (get, gets, put)

import Data.Bifunctor (bimap)
import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Monoid (Last(Last))
import Data.Traversable (for)
import Data.Void (Void, absurd)

import Language.Syrup.Bwd
import Language.Syrup.Va
import Language.Syrup.Ty
import Language.Syrup.Syn
import Language.Syrup.BigArray


------------------------------------------------------------------------------
-- how to invoke the typechecker
------------------------------------------------------------------------------

mkComponent :: CoEnv -> (DEC, String) -> Maybe (Def, String)
            -> (Bool, [String], CoEnv)
mkComponent env (dec, decSrc) mdef =
  case (cookDec dec, mdef) of
    (dec@(Dec (g, ss) ts), Just (def, defSrc)) ->
      case tyM (guts dec def)
             (B0 :< TySOURCE decSrc defSrc)
             (tySt0 {coEnv = env}) of
        Left e ->
          ( False
          , typeErrorReport e ++ ["", g ++ " has been stubbed out."]
          , insertArr (g, stubOut dec) env)
        Right (((ps, rhs), (qs0, qs1), def), st) ->
          let mI  = memIn st
              mO  = memOu st
              ((ta0, k1), tar) =
                glom ([], foldMap support mI) (sched st)
              ((ta1, k2), tat) =
                glom ([], foldMap support (mI ++ ps)) (sched st)
          in  case (tat, foldMap support qs0 `subSet` k1,
                         foldMap support (mO ++ qs1) `subSet` k2) of
                ([], True, True) -> (,,) True [g ++ " is defined."] $
                  let mems = concat $ reverse $ memTy st in
                  let gc = Compo
                        { monick = g
                        , defn = Just def
                        , memTys = mems
                        , inpTys = zipWith (InputWire . pure) ps ss
                        , oupTys = zipWith (mkOutputWire mems) rhs ts
                        , stage0 = plan (Plan mI ta0 qs0)
                        , stage1 = plan (Plan (mI ++ ps) ta1 (mO ++ qs1))
                        }
                  in insertArr (g, gc) env
                e -> -- trace (show (sched st)) $
                  let sin = case e of
                        (_, False, _) ->
                          Stage0 (foldMapSet (yank (sched st))
                                  (diffSet (foldMap support qs0) k1))
                        (_, _, False) ->
                          Stage1 (foldMapSet (yank (sched st))
                                  (diffSet (foldMap support (mO ++ qs1)) k2))
                        _ -> Junk
                  in  ( False
                      , typeErrorReport (B0 :< TySOURCE decSrc defSrc, sin)
                        ++ ["", g ++ " has been stubbed out."]
                      , insertArr (g, stubOut dec) env)
    (dec@(Dec (g, ss) ts), Nothing) ->
      ( False
      , ["", g ++ " has been stubbed out."]
      , insertArr (g, stubOut dec) env)

guts :: Dec -> Def -> TyM (([Pat], [Exp]), ([Pat], [Pat]), TypedDef)
guts (Dec (g, ss) ts) (Def (f, ps) es eqs)
  | f /= g = tyErr (DecDef f g)
  | otherwise = do
  let ss' = map stanTy ss
  let ts' = map stanTy ts
  typs <- local (:< TyINPUTS ss' ps) $ decPats ss' ps
  (qs, tyes) <- local (:< TyOUTPUTS ts' es) $ chkExps (map (, Nothing) ts') es
  st <- get
  eqs <- traverse (traverse (\ eq -> local (:< TyEQN eq) $ chkEqn eq)) eqs
  (qs', (qs0, qs1)) <- foldMap stage ts
  solders qs' qs
  return ((ps, es), (qs0, qs1), Def (f, typs) tyes eqs)
guts (Dec (g, ss) ts) (Stub f msg)
  | f /= g    = tyErr (DecDef f g)
  | otherwise = tyErr (Stubbed msg)

stubOut :: Dec -> Compo
stubOut (Dec (g, ss) ts) = Compo
  { monick = g
  , defn = Nothing
  , memTys = []
  , inpTys = InputWire  Nothing <$> ss
  , oupTys = OutputWire Nothing <$> ts
  , stage0 = const (fmap stub ts0)
  , stage1 = const (fmap stub ts1)
  } where (ts0, ts1) = foldMap splitTy2 ts


decPats :: [Typ] -> [Pat] -> TyM [TypedPat]
decPats [] [] = return []
decPats [] ps = tyErr LongPats
decPats ss [] = tyErr ShortPats
decPats (s : ss) (p : ps) = (:) <$> decPat s p <*> decPats ss ps

decPat :: Typ -> Pat -> TyM TypedPat
decPat s (PVar () x) = PVar s x <$ defineWire (Just s) x
decPat s@(Cable ss) (PCab () ps)
  | length ss == length ps = PCab s <$> decPats ss ps
  | otherwise = tyErr CableWidth
decPat _ (PCab _ _) = tyErr BitCable


------------------------------------------------------------------------------
-- where-equations
------------------------------------------------------------------------------

chkEqn :: Eqn -> TyM TypedEqn
chkEqn eqn@(qs :=: es) = do
  tqs <- traverse defPat qs
  (ps, es) <- chkExps (map (fmap Just) tqs) es
  solders qs ps
  pure (map snd tqs :=: es)

defPat :: Pat -> TyM (Typ, TypedPat)
defPat (PVar () x)  = do
  ty <- defineWire Nothing x
  pure (ty, PVar ty x)
defPat (PCab () ps) = do
  (tys, pats) <- unzip <$> traverse defPat ps
  let ty = Cable tys
  pure (ty, PCab ty pats)

chkExps :: [(Typ, Maybe TypedPat)] -> [Exp] -> TyM ([Pat], [TypedExp])
chkExps []  []       = return ([], [])
chkExps tqs []       = tyErr LongPats
chkExps tqs (e : es) = do
  (ps, tqs, e) <- local (:< TyEXP e (fst <$> tqs)) $ chkExp tqs e
  bimap (ps ++) (e :) <$> chkExps tqs es

solders :: [Pat] -> [Pat] -> TyM ()
solders [] [] = return ()
solders (q : qs) (p : ps) = solder q p >> solders qs ps
solders _ _ = tyErr BUGSolderMismatch

solder :: Pat -> Pat -> TyM ()
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

memRenamings :: [Maybe (Pat' ty String)] -> [OutputWire] -> [(CellName, String)]
memRenamings ps os = concat $ zipWith memRenaming ps os

renameMem :: [(CellName, String)] -> MemoryCell -> MemoryCell
renameMem rho (MemoryCell mc t) = MemoryCell (fmap CellName $ mc >>= flip lookup rho) t

chkExp :: [(Typ, Maybe TypedPat)]
       -> Exp
       -> TyM ([Pat], [(Typ, Maybe TypedPat)], TypedExp)
chkExp ((t,_) : ts) (Var () x) = do
  s <- useWire x
  local (:< TyWIRE x s t) $ tyEq (s, t)
  return ([PVar () x], ts, Var s x)
chkExp tqs e@(App fn es) = do
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
    defineWire (Just (stanTy ty)) w
    return (PVar () w)
  mOu <- for mTy $ \ ty -> do
    w <- wiF
    defineWire (Just (stanTy ty)) w
    return (PVar () w)
  st <- get
  put $ st { memTy = memTys : memTy st
           , memIn = memIn st ++ mIn
           , memOu = memOu st ++ mOu
           }
  let iTy = getInputType <$> inpTys f
  (ps, es) <- local (:< TyAPP f es) $ chkExps (map (\t -> (stanTy t, Nothing)) iTy) es
  let oTy = getOutputType <$> oupTys f
  (qs, (qs0, qs1)) <- foldMap stage oTy
  schedule (qs0 :<- (stage0 f, mIn))
  schedule ((mOu ++ qs1) :<- (stage1 f, mIn ++ ps))
  (qs,,App fn es) <$> yield (map stanTy oTy) tqs
chkExp (tq : tqs) (Cab () es) = do
  sqs <- case tq of
    (Cable ss, Just (PCab _ qs))
      | length ss == length qs -> return (zipWith (\ s q -> (s, Just q)) ss qs)
      | otherwise              -> return (map (, Nothing) ss)
    (Cable ss, Nothing)        -> return (map (, Nothing) ss)
    (Bit _, _) -> tyErr BitCable
    (TyV x, _) -> do
      ss <- traverse (const tyF) es
      tyEq (Cable ss, TyV x)
      return (map (, Nothing) ss)
  (ps, es) <- local (:< TyCAB es (fst <$> sqs)) $ chkExps sqs es
  return ([PCab () ps], tqs, Cab (Cable (map fst sqs)) es)
chkExp [] _ = tyErr ShortPats

yield :: [Typ] -> [(Typ, a)] -> TyM [(Typ, a)]
yield []       tqs = return tqs
yield (s : ss) []  = tyErr ShortPats
yield (s : ss) ((t , q) : tqs) = tyEq (s, t) >> yield ss tqs

stage :: Ty2 -> TyM ([Pat], ([Pat], [Pat]))
stage (TyV x) = absurd x
stage (Bit t) = do
  w <- wiF
  defineWire (Just (Bit ())) w
  return ([PVar () w], case t of {T0 -> ([PVar () w], []); T1 -> ([], [PVar () w])})
stage (Cable ts) = do
  (qs, (qs0, qs1)) <- foldMap stage ts
  return ([PCab () qs], ([PCab () qs0], [PCab () qs1]))

------------------------------------------------------------------------------
-- from raw to cooked Syrup types
------------------------------------------------------------------------------

data Dec
  = Dec (String, [Ty1]) [Ty2]
  deriving Show

cookDec :: DEC -> Dec
cookDec (DEC (f, is) os) =
  Dec (f, fmap (cookTY () id) is) (fmap (cookTY T1 (const T0)) os)

cookTY :: t -> (t -> t) -> TY -> Ty t Void
cookTY t old BIT         = Bit t
cookTY t old (OLD ty)    = cookTY (old t) old ty
cookTY t old (CABLE tys) = Cable (fmap (cookTY t old) tys)

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
    problem (Stubbed msg) = msg
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
    context (Stubbed _) = const []
    context _ = ("" :) . context'
    context' (_ :< TyEQN eq) =
      ["At the time, I was checking this equation:", show eq]
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
      , "  " ++ show e
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
      , "but it carried a signal of type " ++ show s
      , "where a signal of type " ++ show t
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

myCoEnv :: CoEnv
myCoEnv = foldr insertArr emptyCoEnv
  [ ("nand", Compo
      { monick = "nand"
      , defn = Nothing
      , memTys = []
      , inpTys = [ InputWire (Just (PVar () "X")) (Bit ())
                 , InputWire (Just (PVar () "Y")) (Bit ())
                 ]
      , oupTys = [ OutputWire Nothing (Bit T1) ]
      , stage0 = \ [] -> []
      , stage1 = \ i -> case i of
          [V0, _ ] -> [V1]
          [_ , V0] -> [V1]
          [V1, V1] -> [V0]
          _        -> [VQ]
      }
    )
  , ("dff", Compo
      { monick = "dff"
      , defn = Nothing
      , memTys = [MemoryCell (Just $ CellName "Q") (Bit ())]
      , inpTys = [InputWire  (Just (PVar () "D")) (Bit ())]
      , oupTys = [OutputWire (Just (PVar () ("Q", True))) (Bit T0)]
      , stage0 = \ [q] -> [q]
      , stage1 = \ [_, d] -> [d]
      }
    )
  , ("zero", Compo
      { monick = "zero"
      , defn = Nothing
      , memTys = []
      , inpTys = []
      , oupTys = [OutputWire Nothing (Bit T0)]
      , stage0 = \ _ -> [V0]
      , stage1 = \ _ -> []
      }
    )
  ]

emptyTyEnv :: TyEnv
emptyTyEnv = emptyArr

myTyEnv :: TyEnv
myTyEnv = emptyTyEnv

env1, env2, env3, env4, env5, env6, env7, env8, env9 :: CoEnv
(_, _, env1) = mkComponent myCoEnv
  (DEC ("not", [BIT]) [BIT], "!<Bit> -> <Bit>") $ Just
  (Def ("not", [PVar () "x"]) [Var () "y"] $ Just
   [[PVar () "y"] :=: [App "nand" [Var () "x", Var () "x"]]]
  ,"!x = y where  y = nand(x,x)")

(_, _, env2) = mkComponent env1
  (DEC ("and", [BIT, BIT]) [BIT], "<Bit> & <Bit> -> <Bit>") $ Just
  (Def ("and", [PVar () "x", PVar () "y"]) [Var () "b"] $ Just
    [[PVar () "a"] :=: [App "nand" [Var () "x", Var () "y"]]
    ,[PVar () "b"] :=: [App "not" [Var () "a"]]
    ]
  ,"x & y = b where  a = nand(x,y)  b = not(a)")

(_, _, env3) = mkComponent env2
  (DEC ("or", [BIT, BIT]) [BIT], "<Bit> | <Bit> -> <Bit>") $ Just
  (Def ("or", [PVar () "x", PVar () "y"]) [Var () "c"] $ Just
    [[PVar () "c"] :=: [App "nand" [Var () "a", Var () "b"]]
    ,[PVar () "a",PVar () "b"] :=: [App "not" [Var () "x"],App "not" [Var () "y"]]
    ]
  ,"x | y = c where  c = nand(a,b)  a,b = !x,!y")

(_, _, env4) = mkComponent env3
  (DEC ("jkff", [BIT, BIT]) [OLD BIT], "jkff(<Bit>,<Bit>) -> @<Bit>") $ Just
  (Def ("jkff", [PVar () "j", PVar () "k"]) [Var () "q"] $ Just
    [[PVar () "q"] :=: [App "dff" [Var () "d"]]
    ,[PVar () "d"] :=: [App "or"
       [  App "and" [Var () "j", App "not" [Var () "q"]]
       ,  App "and" [Var () "q", App "not" [Var () "k"]]
       ]]
    ]
  ,"jkff(j,k) = q where  q = dff(d)  d = j & !q | q & !k")

(_, _, env5) = mkComponent env4
  (DEC ("ndnff", [BIT]) [OLD BIT], "ndnff(<Bit>) -> @<Bit>") $ Just
  (Def ("ndnff", [PVar () "d"]) [App "not" [Var () "q"]] $ Just
    [[PVar () "q"] :=: [App "dff" [App "not" [Var () "d"]]]
    ]
  ,"ndnff(d) = !q where  q = dff(!d)")

(_, _, env6) = mkComponent env5
  (DEC ("xor", [BIT,BIT]) [BIT], "xor(<Bit>,<Bit>) -> <Bit>") $ Just
  (Def ("xor", [PVar () "x", PVar () "y"])
       [App "or" [ App "and" [App "not" [Var () "x"], Var () "y"]
                 , App "and" [Var () "x", App "not" [Var () "y"]]
                 ]]
       Nothing
  ,"xor(x,y) = !x & y | x & !y")

(_, _, env7) = mkComponent env6
  (DEC ("tff", [BIT]) [OLD BIT], "tff(<Bit>) -> @<Bit>") $ Just
  (Def ("tff", [PVar () "t"]) [Var () "q"] $ Just
    [[PVar () "q"] :=: [App "dff" [Var () "d"]]
    ,[PVar () "d"] :=: [App "xor" [Var () "t", Var () "q"]]
    ]
  ,"tff(t) = q where q = dff(d) d = xor(t,q)")

(_, _, env8) = mkComponent env7
  (DEC ("one", []) [OLD BIT], "one() -> @<Bit>") $ Just
  (Def ("one", []) [App "not" [App "zero" []]] Nothing
  ,"one() = !zero()")

(_, _, env9) = mkComponent env8
  (DEC ("tff2", [BIT]) [OLD BIT], "tff2(<Bit>) -> @<Bit>") $ Just
  (Def ("tff2", [PVar () "t"]) [App "xor" [Var () "q2",Var () "q1"]] $ Just
    [[PVar () "q2"] :=: [App "tff" [App "or" [App "not" [Var () "t"],Var () "q1"]]]
    ,[PVar () "q1"] :=: [App "tff" [App "one" []]]
    ]
  ,"tff2(T) = xor(Q2,Q1) where Q2 = tff(!T | Q1) Q1 = tff(one())")
