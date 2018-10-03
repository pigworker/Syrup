------------------------------------------------------------------------------
-----                                                                    -----
-----     Par: Parsing Syrup                                             -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE PatternGuards, MultiParamTypeClasses #-}

module Par where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import BigArray
import Bwd
import Syn
import Lex


------------------------------------------------------------------------------
-- the monad
------------------------------------------------------------------------------

newtype Par x = Par {par :: ParEn -> ParSt -> Either ParErr (x, ParSt)}

instance Monad Par where
  return x = Par $ \ g s -> Right (x, s)
  Par pa >>= k = Par $ \ g s -> case pa g s of
    Left e -> Left e
    Right (a, s) -> par (k a) g s

instance Monoid (Par x) where
  mempty = Par $ \ g s -> Left mempty
  mappend (Par p1) (Par p2) = Par $ \ g s -> case (p1 g s, p2 g s) of
    (Right xs, _)    -> Right xs
    (_, Right xs)    -> Right xs
    (Left e, Left f) -> Left (e <> f)

data ParEn = ParEn
  {  keywords :: Set String
  ,  context  :: Bwd ParClue
  } deriving Show

data ParSt = ParSt
  {  toksEaten :: Bwd Token
  ,  toksAhead :: [Token]
  } deriving Show

data ParErr
  = Mystery
  | Explanation (Bwd ParClue) (Bwd Token, [Token]) ParYelp
  deriving Show

instance Monoid ParErr where
  mempty = Mystery
  mappend Mystery e = e
  mappend e Mystery = e
  mappend e1@(Explanation tc1 (tz1, _) _) e2@(Explanation tc2 (tz2, _) _) =
    case compare (length tc1, length tz1) (length tc2, length tz2) of
      GT -> e1
      _  -> e2   -- ORLY?

data ParClue
  = SOURCE String
  | SEEKING String
  | BRACKET Bracket (Bwd Token) [Token]
  deriving Show

data ParYelp
  = AARGH
  | UnexpectedEnd
  | ExpectedEnd
  | WantedGot Token Token
  | WantedBrk Bracket Token
  | WantedIdGotKeyword String
  | WantedIdSymbolic Token
  deriving Show


------------------------------------------------------------------------------
-- handy equipment
------------------------------------------------------------------------------

pYelp :: ParYelp -> Par x
pYelp y = Par $ \ g s -> Left $ Explanation
  (context g)
  (toksEaten s, toksAhead s)
  y

pClue :: ParClue -> Par x -> Par x
pClue c = local $ \ g -> g {context = context g :< c}

pTok :: (Token -> ParYelp) -> (Token -> Maybe x) -> Par x
pTok y p = do
  tz <- gets toksEaten
  ts <- gets toksAhead
  case ts of
    t : ts
      | Just x <- p t -> do
        st <- get
        put (st {toksEaten = tz :< t, toksAhead = ts})
        return x
      | otherwise -> pYelp (y t)
    [] -> pYelp UnexpectedEnd

pTokIs :: Token -> Par ()
pTokIs t = pTok (WantedGot t) (guard . (t ==))

pVar :: Par String
pVar = do
  kws <- asks keywords
  let happy (Id x) | not (inSet x kws) = Just x
      happy _ = Nothing
  let moan (Id kw) = WantedIdGotKeyword kw
      moan t = WantedIdSymbolic t
  pClue (SEEKING "variable") (pTok moan happy)

pSpc :: Par ()
pSpc = () <$ (many . pTok (const AARGH) $ \ t -> case t of
  Spc _ -> Just ()
  _     -> Nothing
  )

pEOI :: Par ()
pEOI = do
  ts <- gets toksAhead
  case ts of
    [] -> return ()
    _  -> pYelp ExpectedEnd
  
pBrk :: Bracket -> Par x -> Par x
pBrk b p = do
  tz <- gets toksEaten
  ts <- gets toksAhead
  case ts of
    t@(Bracket b' us) : ts | b == b' -> do
      st <- get
      put (st {toksEaten = B0, toksAhead = us})
      x <- pClue (BRACKET b tz ts) $ do
        id <$ pSpc <*> p <* pSpc <* pEOI
      st <- get
      put (st {toksEaten = tz :< t, toksAhead = ts})
      return x
    t : _ -> pYelp (WantedBrk b t)
    [] -> pYelp UnexpectedEnd

pSep :: Par () -> Par x -> Par [x]
pSep s p = id
  <$> ((:) <$> p <*> many (id <$ pSpc <* s <* pSpc <*> p) <|> pure [])

pPeek :: ([Token] -> Bool) -> Par ()
pPeek ah = do
  ts <- gets toksAhead
  if ah ts then return () else pYelp AARGH


------------------------------------------------------------------------------
-- parsing Syrup
------------------------------------------------------------------------------

pSource :: Par Source
pSource = pClue (SEEKING "Syrup source code") $
  Declaration <$> pDEC <|> Definition <$> pDef

pDEC :: Par DEC
pDEC = pClue (SEEKING "declaration") $ DEC
  <$ pPeek (elem (Sym "->"))
  <*> pLhs "type" pTY <* pSpc <* pTokIs (Sym "->") <* pSpc
  <*> pClue (SEEKING "list of output types") (pSep (pTokIs (Sym ",")) pTY)
  <* pSpc <* pEOI

pDef :: Par Def
pDef = pClue (SEEKING "definition") $ Def
  <$ pPeek (elem (Sym "="))
  <*> pLhs "pattern" pPat <* pSpc <* pTokIs (Sym "=") <* pSpc
  <*> pClue (SEEKING "list of outputs") (pSep (pTokIs (Sym ",")) pExp)
  <* pSpc <*> pEqns

pLhs :: String -> Par x -> Par (String, [x])
pLhs s p = pClue (SEEKING "component template") $
      (,) "not" <$ pTokIs (Sym "!") <* pSpc <*> ((:[]) <$> p)
  <|> (,) "and" <$>
        ((:) <$> p <* pSpc <* pTokIs (Sym "&") <* pSpc <*> ((:[]) <$> p))
  <|> (,) "or" <$>
        ((:) <$> p <* pSpc <* pTokIs (Sym "|") <* pSpc <*> ((:[]) <$> p))
  <|> (,) <$> pVar <* pSpc
          <*> pBrk Round
            (pClue (SEEKING $ "list of input " ++ s ++ "s") $
             pSep (pTokIs (Sym ",")) p)
  <|> pYelp AARGH 

pTY :: Par TY
pTY = pClue (SEEKING "type") $
      BIT <$ pTokIs (Sym "<") <* pTokIs (Id "Bit") <* pTokIs (Sym ">")
  <|> OLD BIT <$ pTokIs (Sym "@<") <* pTokIs (Id "Bit") <* pTokIs (Sym ">")
  <|> OLD <$ pTokIs (Sym "@") <* pSpc <*> pTY
  <|> CABLE <$> pBrk Square (pSep (pTokIs (Sym ",")) pTY)
  <|> pYelp AARGH

pPat :: Par Pat
pPat = pClue (SEEKING "pattern") $
  PVar <$> pVar <|> PCab <$> pBrk Square (pSep (pTokIs (Sym ",")) pPat)

pExp :: Par Exp
pExp = pExpPrec 0

pExpPrec :: Int -> Par Exp
pExpPrec prec = pWee >>= pMore prec

pWee :: Par Exp
pWee = pClue (SEEKING "expression") $
      (pVar >>= \ f -> App f <$ pSpc <*> pBrk Round
        (pClue (SEEKING $ "input for " ++ f) $
         pSep (pTokIs (Sym ",")) pExp))
  <|> Var <$> pVar
  <|> Cab <$> pBrk Square (pSep (pTokIs (Sym ",")) pExp)
  <|> (App "!" . (:[])) <$ pTokIs (Sym "!") <* pSpc <*> pWee
  <|> pBrk Round pExp

pMore :: Int -> Exp -> Par Exp
pMore prec e = ( pSpc *> (
  (App "or" <$ guard (prec <= 1) <* pTokIs (Sym "|") <* pSpc
     <*> (((e:) . (:[])) <$> pExpPrec 1) >>= pMore prec)
  <|>
  (App "and" <$ guard (prec <= 2) <* pTokIs (Sym "&") <* pSpc
     <*> (((e:) . (:[])) <$> pExpPrec 2) >>= pMore prec)  
  ) ) <|> pure e


pEqns :: Par [Eqn]
pEqns = [] <$ pEOI <|>
  id <$ pTokIs (Id "where") <* pSpc <*>
  many (pEqn <* pSpc) <* pEOI

pEqn :: Par Eqn
pEqn = pClue (SEEKING "equation") $
  (:=:) <$> pSep (pTokIs (Sym ",")) pPat <* pSpc <* pTokIs (Sym "=") <* pSpc
        <*> pSep (pTokIs (Sym ",")) pExp


------------------------------------------------------------------------------
-- the top level
------------------------------------------------------------------------------

syrupFile :: String -> [Either ParErr Source]
syrupFile = map syrupSource . lexFile

syrupSource :: (String, [Token]) -> Either ParErr Source
syrupSource (s, ts) = case par pSource en st of
    Left e -> Left e
    Right (x, _) -> Right x
  where
    en = ParEn
      { keywords = syrupKeywords
      , context = B0 :< SOURCE s
      }
    st = ParSt
      { toksEaten = B0
      , toksAhead = ts
      }

syrupKeywords = foldMap singleton
  ["where"]


------------------------------------------------------------------------------
-- boring instances
------------------------------------------------------------------------------

instance Applicative Par where
  pure = return
  (<*>) = ap

instance Functor Par where
  fmap = ap . return

instance Semigroup (Par x) where
  (<>) = mappend
  
instance Alternative Par where
  empty = mempty
  (<|>) = (<>)

instance Semigroup ParErr where
  (<>) = mappend

instance MonadState ParSt Par where
  get   = Par $ \ g s -> Right (s, s)
  put s = Par $ \ g _ -> Right ((), s)

instance MonadReader ParEn Par where
  ask       = Par $ \ g s -> Right (g, s)
  local f p = Par (par p . f)
