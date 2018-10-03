------------------------------------------------------------------------------
-----                                                                    -----
-----     Chk: TypeChecking Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

import Data.Void
import Data.Traversable
import Control.Monad.State

import Debug.Trace

import Va
import Ty
import Syn
import BigArray


------------------------------------------------------------------------------
-- how to invoke the typechecker
------------------------------------------------------------------------------

mkComponent :: CoEnv -> DEC -> Def -> Either TyErr (String, Compo)
mkComponent env dec def =
  case cookDec dec of
    dec@(Dec (g, ss) ts) -> case tyM (guts dec def) (tySt0 {coEnv = env}) of
      Left e -> Left e
      Right ((ps, (qs0, qs1)), st) -> trace (show (ps, sched st, qs0, qs1)) $
        let mI  = memIn st
            mO  = memOu st
            ((ta0, k1), tar) =
              glom ([], foldMap support mI) (sched st)
            ((ta1, k2), tat) =
              glom ([], foldMap support (mI ++ ps)) (sched st)
        in  case (tat, foldMap support qs0 `subSet` k1,
                       foldMap support (mO ++ qs1) `subSet` k2) of
              ([], True, True) -> Right . (,) g $ Compo
                { memTys = memTy st
                , inpTys = ss
                , oupTys = ts
                , stage0 = plan (Plan mI ta0 qs0)
                , stage1 = plan (Plan (mI ++ ps) ta1 (mO ++ qs1))
                }
              x -> trace (show x) $ Left DefLoop

guts :: Dec -> Def -> TyM ([Pat], ([Pat], [Pat]))
guts (Dec (g, ss) ts) (Def (f, ps) es eqs)
  | f /= g = tyErr (DecDef f g)
  | otherwise = do
  let ss' = map (stanTy1 T1) ss
  let ts' = map (stanTy2 T1) ts
  decPats ss' ps
  qs <- chkExps ts' es
  st <- get
  True <- trace ("preq: " ++ show st) $ return True
  foldMap chkEqn eqs
  (qs', (qs0, qs1)) <- foldMap (stage T1) ts
  solders qs' qs
  return (ps, (qs0, qs1))

decPats :: [Typ] -> [Pat] -> TyM ()
decPats [] [] = return ()
decPats [] ps = tyErr LongPats
decPats ss [] = tyErr ShortPats
decPats (s : ss) (p : ps) = do
  decPat s p
  decPats ss ps

decPat :: Typ -> Pat -> TyM ()
decPat s (PVar x) = () <$ defineWire (Just s) x
decPat (Cable ss) (PCab ps)
  | length ss == length ps = decPats ss ps
  | otherwise = tyErr CableWidth
decPat _ (PCab _) = tyErr BitCable
  

------------------------------------------------------------------------------
-- where-equations
------------------------------------------------------------------------------

chkEqn :: Eqn -> TyM ()
chkEqn eqn@(qs :=: es) = trace (show eqn) $ do
  ts <- traverse defPat qs
  ps <- chkExps ts es
  solders qs ps

defPat :: Pat -> TyM Typ
defPat (PVar x)  = defineWire Nothing x
defPat (PCab ps) = Cable <$> traverse defPat ps

chkExps :: [Typ] -> [Exp] -> TyM [Pat]
chkExps [] []       = return []
chkExps ts []       = tyErr LongPats
chkExps ts (e : es) = do
  (ps, ts) <- chkExp ts e
  (ps ++) <$> chkExps ts es

solders :: [Pat] -> [Pat] -> TyM ()
solders [] [] = return ()
solders (q : qs) (p : ps) = solder q p >> solders qs ps

solder :: Pat -> Pat -> TyM ()
solder (PCab qs) (PCab ps) = solders qs ps
solder q p = schedule ([q] :<- (id, [p]))


------------------------------------------------------------------------------
-- expressions
------------------------------------------------------------------------------

chkExp :: [Typ] -> Exp -> TyM ([Pat], [Typ])
chkExp (t : ts) (Var x) = do
  s <- useWire x
  True <- trace (x ++ " : " ++ show s) $ return True
  tyLe (s, t)
  return ([PVar x], ts)
chkExp ts e@(App f es) = do
  env <- trace (show (ts, e)) $ gets coEnv
  f <- case findArr f env of
    Nothing -> tyErr (Don'tKnow f)
    Just f  -> return f
  u <- tiF  -- when f runs
  let mTy  = memTys f
  mIn <- for mTy $ \ ty -> do
    w <- wiF
    defineWire (Just (stanTy1 T0 ty)) w
    return (PVar w)
  mOu <- for mTy $ \ ty -> do
    w <- wiF
    defineWire (Just (stanTy1 T1 ty)) w
    return (PVar w)
  st <- get
  put (st { memTy = memTy st ++ mTy
          , memIn = memIn st ++ mIn
          , memOu = memOu st ++ mOu } )
  ps <- chkExps (map (stanTy1 u) (inpTys f)) es
  (qs, (qs0, qs1)) <- foldMap (stage u) (oupTys f)
  schedule (qs0 :<- (stage0 f, mIn))
  schedule ((mOu ++ qs1) :<- (stage1 f, mIn ++ ps))
  (,) qs <$> yield (map (stanTy2 u) (oupTys f)) ts
chkExp (t : ts) (Cab es) = do
  ss <- case t of
    Cable ss -> return ss
    Bit _ -> tyErr BitCable
    TyV x -> do
      ss <- traverse (const tyF) es
      tyLe (Cable ss, TyV x)
      return ss
  ps <- chkExps ss es
  return ([PCab ps], ts)
chkExp [] _ = tyErr ShortPats

yield :: [Typ] -> [Typ] -> TyM [Typ]
yield [] ts       = return ts
yield (s : ss) [] = tyErr ShortPats
yield (s : ss) (t : ts) = tyLe (s, t) >> yield ss ts

stage :: Tim -> Ty2 -> TyM ([Pat], ([Pat], [Pat]))
stage u (Bit T0) = do
  w <- wiF
  defineWire (Just (Bit T0)) w
  return ([PVar w], ([PVar w], []))
stage u (Bit T1) = do
  w <- wiF
  defineWire (Just (Bit u)) w
  return ([PVar w], ([], [PVar w]))
stage u (Cable ts) = do
  (qs, (qs0, qs1)) <- foldMap (stage u) ts
  return ([PCab qs], ([PCab qs0], [PCab qs1]))


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
-- experiments
------------------------------------------------------------------------------

myCoEnv :: CoEnv
myCoEnv = foldr insertArr emptyArr
  [ ("nand", Compo
      { memTys = []
      , inpTys = [Bit (), Bit ()]
      , oupTys = [Bit T1]
      , stage0 = \ [] -> []
      , stage1 = \ i -> case i of
          [B0, B0] -> [B1]
          [B0, B1] -> [B1]
          [B1, B0] -> [B1]
          [B1, B1] -> [B0]
      }
    )
  , ("dff", Compo
      { memTys = [Bit ()]
      , inpTys = [Bit ()]
      , oupTys = [Bit T0]
      , stage0 = \ [q] -> [q]
      , stage1 = \ [_, d] -> [d]
      }
    )
  ]

Right chkNot = mkComponent myCoEnv
  (DEC ("not", [BIT]) [BIT])
  (Def ("not", [PVar "x"]) [Var "y"]
    [[PVar "y"] :=: [App "nand" [Var "x", Var "x"]]])

env1 = insertArr chkNot myCoEnv

Right chkAnd = mkComponent env1
  (DEC ("and", [BIT, BIT]) [BIT])
  (Def ("and", [PVar "x", PVar "y"]) [Var "b"]
    [[PVar "a"] :=: [App "nand" [Var "x", Var "y"]]
    ,[PVar "b"] :=: [App "not" [Var "a"]]
    ])

env2 = insertArr chkAnd env1
Right chkOr = mkComponent env2
  (DEC ("or", [BIT, BIT]) [BIT])
  (Def ("or", [PVar "x", PVar "y"]) [Var "c"]
    [[PVar "c"] :=: [App "nand" [Var "a", Var "b"]]
    ,[PVar "a",PVar "b"] :=: [App "not" [Var "x"],App "not" [Var "y"]]
    ])

env3 = insertArr chkOr env2
Right chkJKff = mkComponent env3
  (DEC ("jkff", [BIT, BIT]) [OLD BIT])
  (Def ("jkff", [PVar "j", PVar "k"]) [Var "q"]
    [[PVar "q"] :=: [App "dff" [Var "d"]]
    ,[PVar "d"] :=: [App "or"
       [  App "and" [Var "j", App "not" [Var "q"]]
       ,  App "and" [Var "q", App "not" [Var "k"]]
       ]]
    ])
