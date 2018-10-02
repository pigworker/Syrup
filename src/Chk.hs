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
mkComponent env decl (Def (f, ps) es qs) = case cookDec decl of
  Dec (g, ss) ts
    | f /= g -> Left (DecDef g f)
    | otherwise -> undefined


------------------------------------------------------------------------------
-- where-equations
------------------------------------------------------------------------------

chkEqn :: Eqn -> TyM ()
chkEqn (ps :=: es) = do
  ts <- traverse defPat ps
  chkExps ts es

defPat :: Pat -> TyM Typ
defPat (PVar x)  = defineWire x
defPat (PCab ps) = Cable <$> traverse defPat ps

chkExps :: [Typ] -> [Exp] -> TyM ()
chkExps [] []       = return ()
chkExps ts (e : es) = do
  ts <- chkExp ts e
  chkExps ts es


------------------------------------------------------------------------------
-- where-equations
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


