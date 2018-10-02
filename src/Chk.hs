------------------------------------------------------------------------------
-----                                                                    -----
-----     Chk: TypeChecking Syrup                                        -----
-----                                                                    -----
------------------------------------------------------------------------------

import Data.Void
import Control.Monad.State

import Ty
import Syn
import BigArray


------------------------------------------------------------------------------
-- how to invoke the typechecker
------------------------------------------------------------------------------

mkComponent :: CoEnv -> Dec TY -> Def -> Either TyErr (String, Compo)
mkComponent env dec def =
  case cookDec dec of
    dec@(Dec (g, ss) ts) -> case tyM (guts dec def) (tySt0 {coEnv = env}) of
      Left e -> Left e
      Right ((), st) -> Right . (,) g $ Compo
        { memTys = [] -- FIXME
        , inpTys = ss
        , oupTys = ts
        }

guts :: Dec Ty2 -> Def -> TyM ()
guts (Dec (g, ss) ts) (Def (f, ps) es qs)
  | f /= g = tyErr (DecDef f g)
  | otherwise = do
  let ss' = map (stanTy2 T1) ss
  let ts' = map (stanTy2 T1) ts
  decPats ss' ps
  chkExps ts' es
  foldMap chkEqn qs


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
chkEqn (ps :=: es) = do
  ts <- traverse defPat ps
  chkExps ts es

defPat :: Pat -> TyM Typ
defPat (PVar x)  = defineWire Nothing x
defPat (PCab ps) = Cable <$> traverse defPat ps

chkExps :: [Typ] -> [Exp] -> TyM ()
chkExps [] []       = return ()
chkExps ts []       = tyErr LongPats
chkExps ts (e : es) = do
  ts <- chkExp ts e
  chkExps ts es


------------------------------------------------------------------------------
-- expressions
------------------------------------------------------------------------------

chkExp :: [Typ] -> Exp -> TyM [Typ]
chkExp (t : ts) (Var x) = do
  s <- useWire x
  tyLe (s, t)
  return ts
chkExp ts (App f es) = do
  st <- get
  f <- case findArr f (coEnv st) of
    Nothing -> tyErr (Don'tKnow f)
    Just f  -> return f
  u <- tiF  -- when f runs
  let ms = map stanTy1 (memTys f)
  let rs = map (stanTy2 u) (inpTys f)
  let ss = map (stanTy2 u) (oupTys f)
  chkExps rs es
  yield ss ts
chkExp (t : ts) (Cab es) = do
  ss <- case t of
    Cable ss -> return ss
    Bit _ -> tyErr BitCable
    TyV x -> do
      ss <- traverse (const tyF) es
      tyLe (Cable ss, TyV x)
      return ss
  chkExps ss es
  return ts
chkExp [] _ = tyErr ShortPats



yield :: [Typ] -> [Typ] -> TyM [Typ]
yield [] ts       = return ts
yield (s : ss) [] = tyErr ShortPats
yield (s : ss) (t : ts) = tyLe (s, t) >> yield ss ts


------------------------------------------------------------------------------
-- from raw to cooked Syrup types
------------------------------------------------------------------------------

cookDec :: Dec TY -> Dec Ty2
cookDec = fmap (cookTY T1)

cookTY :: Ti Void -> TY -> Ty2
cookTY t BIT         = Bit t
cookTY t (OLD ty)    = cookTY T0 ty
cookTY t (CABLE tys) = Cable (fmap (cookTY t) tys)


------------------------------------------------------------------------------
-- experiments
------------------------------------------------------------------------------

myCoEnv :: CoEnv
myCoEnv = foldr insertArr emptyArr
  [ ("nand", Compo
      { memTys = []
      , inpTys = [Bit T1, Bit T1]
      , oupTys = [Bit T1]
      }
    )
  , ("dff", Compo
      { memTys = [Bit ()]
      , inpTys = [Bit T1]
      , oupTys = [Bit T0]
      }
    )
  ]

chkNot = tyM (guts
  (cookDec (Dec ("not", [BIT]) [BIT]))
  (Def ("not", [PVar "x"]) [Var "y"]
    [[PVar "y"] :=: [App "nand" [Var "x", Var "x"]]])
  )
  (tySt0 {coEnv = myCoEnv})
