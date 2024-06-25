------------------------------------------------------------------------------
-----                                                                    -----
-----     Par: Parsing Syrup                                             -----
-----                                                                    -----
------------------------------------------------------------------------------

{-# LANGUAGE PatternGuards, MultiParamTypeClasses #-}

module Language.Syrup.Par where

import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import Language.Syrup.BigArray
import Language.Syrup.Bwd
import Language.Syrup.Syn
import Language.Syrup.Lex


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
      case compare (measure tc1 tz1, perplexity tc2)
                   (measure tc2 tz2, perplexity tc1) of
        GT -> e1
        _  -> e2   -- ORLY?
    where
      perplexity :: Bwd ParClue -> Int
      perplexity B0 = 0
      perplexity (g :< SEEKING _) = 1 + perplexity g
      perplexity (g :< _) = perplexity g

measure :: Bwd ParClue -> Bwd Token -> Int
measure B0 tz1 = sum (fmap tokSize tz1)
measure (g :< BRACKET _ tz _) tz1 =
  sum (fmap tokSize tz1) + 1 + measure g tz
measure (g :< _) tz = measure g tz


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

pBrk :: Bracket -> ParClue -> Par x -> Par x
pBrk b c p = do
  tz <- gets toksEaten
  ts <- gets toksAhead
  case ts of
    t@(Bracket b' us) : ts | b == b' -> do
      st <- get
      put (st {toksEaten = B0, toksAhead = us})
      x <- pClue (BRACKET b tz ts) . pClue c $ do
        id <$ pSpc <*> p <* pSpc <* pEOI
      st <- get
      put (st {toksEaten = tz :< t, toksAhead = ts})
      return x
    t : _ -> pYelp (WantedBrk b t)
    [] -> pYelp UnexpectedEnd

pAll :: Par x -> Par [x]
pAll p = pSpc *> ((:) <$> p <*> pAll p <|> [] <$ pEOI)

pAllSep :: Par () -> Par x -> Par [x]
pAllSep s p = (:) <$> p <* pSpc <*> pRest <|> [] <$ pSpc <* pEOI where
  pRest = (:) <$ s <* pSpc <*> p <* pSpc <*> pRest <|> [] <$ pEOI

pSep :: Par () -> Par x -> Par [x]
pSep s p = ((:) <$> p <*> many (id <$ pSpc <* s <* pSpc <*> p) <|> pure [])

pSep1 :: Par () -> Par x -> Par [x]
pSep1 s p = (:) <$> p <*> many (id <$ pSpc <* s <* pSpc <*> p)

pPeek :: ([Token] -> Bool) -> Par ()
pPeek ah = do
  ts <- gets toksAhead
  if ah ts then return () else pYelp AARGH


------------------------------------------------------------------------------
-- parsing Syrup
------------------------------------------------------------------------------

pSource :: Par SourceC
pSource = pClue (SEEKING "Syrup source code") $
      Declaration <$> pDEC
  <|> TypeAlias  <$> pTYA
  <|> Definition <$> pDef
  <|> Experiment <$> pEXPT
  <|> pYelp AARGH

pDEC :: Par DECC
pDEC = pClue (SEEKING "a declaration") $ DEC
  <$ pPeek (elem (Sym "->"))
  <*> pLhs "type" pTY <* pSpc <* pTokIs (Sym "->") <* pSpc
  <*> pClue (SEEKING "list of output types") (pAllSep (pTokIs (Sym ",")) pTY)

pDef :: Par Def
pDef = pClue (SEEKING "a definition") $ Def
  <$ pPeek (elem (Sym "="))
  <*> pLhs "pattern" pPat <* pSpc <* pTokIs (Sym "=") <* pSpc
  <*> pClue (SEEKING "list of outputs") (pSep (pTokIs (Sym ",")) pExp)
  <* pSpc <*> pEqns

pLhs :: String -> Par x -> Par (String, [x])
pLhs s p = pClue (SEEKING "a component template") $
      (,) "not" <$ pTokIs (Sym "!") <* pSpc <*> ((:[]) <$> p)
  <|> (,) "and" <$>
        ((:) <$> p <* pSpc <* pTokIs (Sym "&") <* pSpc <*> ((:[]) <$> p))
  <|> (,) "or" <$>
        ((:) <$> p <* pSpc <* pTokIs (Sym "|") <* pSpc <*> ((:[]) <$> p))
  <|> (,) <$> pVar <* pSpc
          <*> pBrk Round (SEEKING $ "list of input " ++ s ++ "s")
             (  pSep (pTokIs (Sym ",")) p)
  <|> pYelp AARGH

-- non terminals
pNT :: Par TYC
pNT = BIT   <$  pTokIs (Id "Bit")
  <|> TYVAR <$> pVar

pTY :: Par TYC
pTY = pClue (SEEKING "a type") $
      pTokIs (Sym "<") *> pNT <* pTokIs (Sym ">")
  <|> OLD <$ pTokIs (Sym "@<") <*> pNT <* pTokIs (Sym ">")
  <|> OLD <$ pTokIs (Sym "@") <* pSpc <*> pTY
  <|> CABLE <$> pBrk Square  (SEEKING "cable contents")
                  (pAllSep (pTokIs (Sym ",")) pTY)
  <|> TYVAR <$> pVar
  <|> pYelp AARGH

pTYA :: Par (String, TYC)
pTYA = pTokIs (Id "type") *> pSpc *>
  pClue (SEEKING "a type alias")
    ( (,) <$
      pTokIs (Sym "<") <*> pVar <* pTokIs (Sym ">")
      <* pSpc <* pTokIs (Sym "=") <* pSpc
      <*> pTY
    )

pPat :: Par Pat
pPat = pClue (SEEKING "a pattern") $
  PVar () <$> pVar
  <|> PCab () <$> pBrk Square (SEEKING "cable contents")
         (pAllSep (pTokIs (Sym ",")) pPat)
  <|> pYelp AARGH

pExp :: Par Exp
pExp = pExpPrec 0

pExpPrec :: Int -> Par Exp
pExpPrec prec = pWee >>= pMore prec

pWee :: Par Exp
pWee = pClue (SEEKING "an expression") $
      (pVar >>= \ f -> App f <$ pSpc <*> pBrk Round
        (SEEKING $ "input for " ++ f) (pAllSep (pTokIs (Sym ",")) pExp))
  <|> Var () <$> pVar <* pPeek notApp
  <|> Cab () <$> pBrk Square (SEEKING "cable contents")
                 (pSep (pTokIs (Sym ",")) pExp)
  <|> (App "not" . (:[])) <$ pTokIs (Sym "!") <* pSpc <*> pWee
  <|> pBrk Round (SEEKING "an expression in parentheses") pExp
  <|> pYelp AARGH

notApp :: [Token] -> Bool
notApp (Spc _ : ts) = notApp ts
notApp (Bracket Round _ : _) = False
notApp _ = True

pMore :: Int -> Exp -> Par Exp
pMore prec e = ( pSpc *> (
  (App "or" <$ guard (prec <= 1) <* pTokIs (Sym "|") <* pSpc
     <*> (((e:) . (:[])) <$> pExpPrec 1) >>= pMore prec)
  <|>
  (App "and" <$ guard (prec <= 2) <* pTokIs (Sym "&") <* pSpc
     <*> (((e:) . (:[])) <$> pExpPrec 2) >>= pMore prec)
  ) ) <|> pure e

pEqns :: Par (Maybe [Eqn])
pEqns =
  Just <$ pTokIs (Id "where") <* pSpc <*> pAll pEqn
  <|> Nothing <$ pEOI

pEqn :: Par Eqn
pEqn = pClue (SEEKING "an equation") $
  (:=:) <$> pSep1 (pTokIs (Sym ",")) pPat <* pSpc <* pTokIs (Sym "=") <* pSpc
        <*> pClue (SEEKING "some expressions") (pSep1 (pTokIs (Sym ",")) pExp)


------------------------------------------------------------------------------
-- parsing experiments
------------------------------------------------------------------------------

pEXPT :: Par EXPT
pEXPT =
  Display <$ pTokIs (Id "display") <* pSpc <*> pVar <* pSpc <* pEOI
  <|> pTokIs (Id "experiment") *> pSpc *>
  pClue (SEEKING "an experiment")
  (    Tabulate <$> pVar <* pSpc <* pEOI
  <|>  Simulate <$> pVar <* pSpc <*> pMem <* pSpc <*>
         pBrk Round (SEEKING "a sequence of test inputs")
           (pAllSep (pTokIs (Sym ";")) pVas)
        <* pSpc <* pEOI
  <|>  Bisimilarity <$> pVar <* pSpc <* pTokIs (Sym "=") <* pSpc
         <*> pVar <* pSpc <* pEOI
  )

pMem :: Par [Va]
pMem = id <$> pBrk Curly (SEEKING "the initial memory contents") pVas
   <|> pure []

pVas :: Par [Va]
pVas = (:) <$> pVa <* pSpc <*> pVas <|> pure []

pVa :: Par Va
pVa = V0 <$ pTokIs (Sym "0") <|> V1 <$ pTokIs (Sym "1")
  <|> VC <$> pBrk Square (SEEKING "a cable of test inputs") pVas


------------------------------------------------------------------------------
-- the top level
------------------------------------------------------------------------------

syrupFile :: String -> [Either [String] (SourceC, String)]
syrupFile = map syrupSource . lexFile

syrupSource :: (String, [Token]) -> Either [String] (SourceC, String)
syrupSource (s, ts) = case par pSource en st of
    Left e -> Left (syntaxError e)
    Right (x, _) -> Right (x, s)
  where
    en = ParEn
      { keywords = syrupKeywords
      , context = B0 :< SOURCE s
      }
    st = ParSt
      { toksEaten = B0
      , toksAhead = ts
      }

syrupKeywords :: Set String
syrupKeywords = foldMap singleton
  ["where", "experiment", "type"]


------------------------------------------------------------------------------
-- syntax errors
------------------------------------------------------------------------------

syntaxError :: ParErr -> [String]
syntaxError Mystery =
  ["Something is wrong but I can't quite put my finger on it."]
syntaxError (Explanation cz tzs y) = concat
  [ preamble, [""]
  , ["I got stuck here: "]
  , ["  " ++ whereWasI cz (cur tzs " {HERE} ") ]
  , seeking cz
  , ["At that point, " ++ yelp y]
  ]
  where
    getSrc (SOURCE src) = Last (Just (lines src))
    getSrc _ = Last Nothing
    preamble = case foldMap getSrc cz of
      (Last (Just ls)) ->
        "I was trying to make sense of the following code:" : "" : ls
      _ ->
        ["I can't remember what you wrote."]
    cur (tz, ts) m = foldMap show tz ++ m ++ foldMap show ts
    whereWasI B0 w = w
    whereWasI _ w | length w >= 40 = "... " ++ w ++ " ..."
    whereWasI (cz :< BRACKET b tz ts) w =
      whereWasI cz (cur (tz, ts) (o ++ w ++ c)) where (o, c) = brackets b
    whereWasI (cz :< _) w = whereWasI cz w
    seeking B0 = ["I wish I knew what I was looking for."]
    seeking (cz :< SEEKING x) = ["I was looking for " ++ x ++ "."]
    seeking (cz :< _) = seeking cz
    yelp AARGH = "I didn't know where to begin."
    yelp UnexpectedEnd = "I ran out of input when I needed more."
    yelp ExpectedEnd =
      "it made sense, but then I found stuff I didn't expect."
    yelp (WantedGot w g) = concat
      ["I was hoping for ", show w, " but I found ", show g
      ," which perplexed me."]
    yelp (WantedBrk b t) = concat
      ["I was hoping for ", o, "..", c, " but I found ", show t
      ," which vexed me."] where (o, c) = brackets b
    yelp (WantedIdGotKeyword k) = concat
      ["I was hoping for a variable, but you used ", k
      ," which is a keyword. Sorry."]
    yelp (WantedIdSymbolic t) = concat
      ["I was hoping for a variable, but I found ", show t
      ," which doesn't look like a variable."]



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
