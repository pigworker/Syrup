------------------------------------------------------------------------------
-----                                                                    -----
-----     Chk: TypeChecking Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

module Chk where

import Data.Void
import Data.Char
import Data.List
import Data.Traversable
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid

import Debug.Trace

import Bwd
import Va
import Ty
import Syn
import BigArray


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
        Right ((ps, (qs0, qs1)), st) ->
          let mI  = memIn st
              mO  = memOu st
              ((ta0, k1), tar) =
                glom ([], foldMap support mI) (sched st)
              ((ta1, k2), tat) =
                glom ([], foldMap support (mI ++ ps)) (sched st)
          in  case (tat, foldMap support qs0 `subSet` k1,
                         foldMap support (mO ++ qs1) `subSet` k2) of
                ([], True, True) -> (,,) True [g ++ " is defined."] $
                  let gc = Compo
                        { monick = g
                        , memTys = memTy st
                        , inpTys = ss
                        , oupTys = ts
                        , stage0 = plan (Plan mI ta0 qs0)
                        , stage1 = plan (Plan (mI ++ ps) ta1 (mO ++ qs1))
                        }
                  in  insertArr (g, gc) env
                e -> trace (show (sched st)) $
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

guts :: Dec -> Def -> TyM ([Pat], ([Pat], [Pat]))
guts (Dec (g, ss) ts) (Def (f, ps) es eqs)
  | f /= g = tyErr (DecDef f g)
  | otherwise = do
  let ss' = map stanTy ss
  let ts' = map stanTy ts
  local (:< TyINPUTS ss' ps) $ decPats ss' ps
  qs <- local (:< TyOUTPUTS ts' es) $ chkExps ts' es
  st <- get
  foldMap (\ eq -> local (:< TyEQN eq) $ chkEqn eq) eqs
  (qs', (qs0, qs1)) <- foldMap stage ts
  solders qs' qs
  return (ps, (qs0, qs1))

stubOut :: Dec -> Compo
stubOut (Dec (g, ss) ts) = Compo
  { monick = g
  , memTys = []
  , inpTys = ss
  , oupTys = ts
  , stage0 = const (fmap stub ts0)
  , stage1 = const (fmap stub ts1)
  } where (ts0, ts1) = foldMap splitTy2 ts
  

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
chkEqn eqn@(qs :=: es) = do
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
  (ps, ts) <- local (:< TyEXP e ts) $ chkExp ts e
  (ps ++) <$> chkExps ts es

solders :: [Pat] -> [Pat] -> TyM ()
solders [] [] = return ()
solders (q : qs) (p : ps) = solder q p >> solders qs ps
solders _ _ = tyErr BUGSolderMismatch

solder :: Pat -> Pat -> TyM ()
solder (PCab qs) (PCab ps) = solders qs ps
solder q p = schedule ([q] :<- (id, [p]))


------------------------------------------------------------------------------
-- expressions
------------------------------------------------------------------------------

chkExp :: [Typ] -> Exp -> TyM ([Pat], [Typ])
chkExp (t : ts) (Var x) = do
  s <- useWire x
  local (:< TyWIRE x s t) $ tyEq (s, t)
  return ([PVar x], ts)
chkExp ts e@(App f es) = do
  env <- gets coEnv
  f <- case findArr f env of
    Nothing -> tyErr (Don'tKnow f)
    Just f  -> return f
  let mTy  = memTys f
  mIn <- for mTy $ \ ty -> do
    w <- wiF
    defineWire (Just (stanTy ty)) w
    return (PVar w)
  mOu <- for mTy $ \ ty -> do
    w <- wiF
    defineWire (Just (stanTy ty)) w
    return (PVar w)
  st <- get
  put (st { memTy = memTy st ++ mTy
          , memIn = memIn st ++ mIn
          , memOu = memOu st ++ mOu } )
  ps <- local (:< TyAPP f es) $ chkExps (map stanTy (inpTys f)) es
  (qs, (qs0, qs1)) <- foldMap stage (oupTys f)
  schedule (qs0 :<- (stage0 f, mIn))
  schedule ((mOu ++ qs1) :<- (stage1 f, mIn ++ ps))
  (,) qs <$> yield (map stanTy (oupTys f)) ts
chkExp (t : ts) (Cab es) = do
  ss <- case t of
    Cable ss -> return ss
    Bit _ -> tyErr BitCable
    TyV x -> do
      ss <- traverse (const tyF) es
      tyEq (Cable ss, TyV x)
      return ss
  ps <- local (:< TyCAB es ss) $ chkExps ss es
  return ([PCab ps], ts)
chkExp [] _ = tyErr ShortPats

yield :: [Typ] -> [Typ] -> TyM [Typ]
yield [] ts       = return ts
yield (s : ss) [] = tyErr ShortPats
yield (s : ss) (t : ts) = tyEq (s, t) >> yield ss ts

stage :: Ty2 -> TyM ([Pat], ([Pat], [Pat]))
stage (Bit t) = do
  w <- wiF
  defineWire (Just (Bit ())) w
  return ([PVar w], case t of {T0 -> ([PVar w], []); T1 -> ([], [PVar w])})
stage (Cable ts) = do
  (qs, (qs0, qs1)) <- foldMap stage ts
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
-- error reporting
------------------------------------------------------------------------------

typeErrorReport :: (Bwd TyClue, TyErr) -> [String]
typeErrorReport (cz, e) = concat
  [ preamble, [""]
  , problem e, [""]
  , context cz
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
    context (_ :< TyEQN eq) =
      ["At the time, I was checking this equation:", show eq]
    context (_ :< TyOUTPUTS ts es) =
      [ "I was trying to get outputs"
      , "  " ++ csepShow ts
      , "from expressions"
      , "  " ++ csepShow es
      , "at the time."
      ]
    context (_ :< TyINPUTS ts ps) =
      [ "I was trying to fit inputs"
      , "  " ++ csepShow ts
      , "into the patterns"
      , "  " ++ csepShow ps
      , "at the time."
      ]
    context (_ :< TyEXP e ts) =
      [ "I was hoping to get the beginning of these"
      , "  " ++ csepShow ts
      , "from the expression"
      , "  " ++ show e
      , "at the time."
      ]
    context (_ :< TyCAB es ts) =
      [ "I was trying to make a cable of these"
      , "  " ++ csepShow ts
      , "from the expressions"
      , "  " ++ csepShow es
      , "at the time."
      ]
    context (_ :< TyAPP c es) =
      [ "I was trying to use " ++ monick c ++ " which wants input types"
      , "  " ++ csepShow (inpTys c)
      , "but feeding in these expressions"
      , "  " ++ csepShow es
      , "at the time."
      ]
    context (g :< TyWIRE x s t) =
      [ "I was trying to connect " ++ ww x
      , "but it carried a signal of type " ++ show s
      , "where a signal of type " ++ show t
      , "was expected."
      ] ++ context g
      where
        ww (c : _) = "wire x"
        ww _       = "a wire"
    context _ = ["I can't remember any more about what I thought I was doing."]

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

myCoEnv :: CoEnv
myCoEnv = foldr insertArr emptyArr
  [ ("nand", Compo
      { monick = "nand"
      , memTys = []
      , inpTys = [Bit (), Bit ()]
      , oupTys = [Bit T1]
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
      , memTys = [Bit ()]
      , inpTys = [Bit ()]
      , oupTys = [Bit T0]
      , stage0 = \ [q] -> [q]
      , stage1 = \ [_, d] -> [d]
      }
    )
  , ("zero", Compo
      { monick = "zero"
      , memTys = []
      , inpTys = []
      , oupTys = [Bit T0]
      , stage0 = \ _ -> [V0]
      , stage1 = \ _ -> []
      }
    )
  ]

(_, _, env1) = mkComponent myCoEnv
  (DEC ("not", [BIT]) [BIT], "!<Bit> -> <Bit>") $ Just
  (Def ("not", [PVar "x"]) [Var "y"]
    [[PVar "y"] :=: [App "nand" [Var "x", Var "x"]]]
  ,"!x = y where  y = nand(x,x)")

(_, _, env2) = mkComponent env1
  (DEC ("and", [BIT, BIT]) [BIT], "<Bit> & <Bit> -> <Bit>") $ Just
  (Def ("and", [PVar "x", PVar "y"]) [Var "b"]
    [[PVar "a"] :=: [App "nand" [Var "x", Var "y"]]
    ,[PVar "b"] :=: [App "not" [Var "a"]]
    ]
  ,"x & y = b where  a = nand(x,y)  b = not(a)")

(_, _, env3) = mkComponent env2
  (DEC ("or", [BIT, BIT]) [BIT], "<Bit> | <Bit> -> <Bit>") $ Just
  (Def ("or", [PVar "x", PVar "y"]) [Var "c"]
    [[PVar "c"] :=: [App "nand" [Var "a", Var "b"]]
    ,[PVar "a",PVar "b"] :=: [App "not" [Var "x"],App "not" [Var "y"]]
    ]
  ,"x | y = c where  c = nand(a,b)  a,b = !x,!y")

(_, _, env4) = mkComponent env3
  (DEC ("jkff", [BIT, BIT]) [OLD BIT], "jkff(<Bit>,<Bit>) -> @<Bit>") $ Just
  (Def ("jkff", [PVar "j", PVar "k"]) [Var "q"]
    [[PVar "q"] :=: [App "dff" [Var "d"]]
    ,[PVar "d"] :=: [App "or"
       [  App "and" [Var "j", App "not" [Var "q"]]
       ,  App "and" [Var "q", App "not" [Var "k"]]
       ]]
    ]
  ,"jkff(j,k) = q where  q = dff(d)  d = j & !q | q & !k")
