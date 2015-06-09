module Parser (pExpr, pVar, pSystem) where

import Text.Parsec
import Expr
import Term
    
pSpace = many $ oneOf " \t\n"
         
pVar :: Parsec String () Var
pVar = do
    x  <- oneOf $ ['a' .. 'z']
    xs <- many $ oneOf $ '\'':['a' .. 'z'] ++ ['0' .. '9']
    return $ x:xs

pAtom :: Parsec String () (Expr Var)
pAtom =
    try (pSpace >> pVar >>= (return . Variable)) <|>
    try (do pSpace >> char '\\' >> pSpace
            v <- pVar
            pSpace >> char '.' >> pSpace
            l <- pExpr
            return $ Abstraction v l) <|>
    (do pSpace >> char '(' >> pSpace
        l <- pExpr
        pSpace >> char ')'
        return l)

pExpr :: Parsec String () (Expr Var)
pExpr = do
    a  <- pAtom
    as <- many $ try $ (many1 $ oneOf " \t\n") >> pAtom
    return $ Apply (a:as)

pTSpace = many $ oneOf " \t"
          
pSystem :: Parsec String () [(Term, Term)]
pSystem = do
  e  <- pEquation
  el <- many $ try $ char '\n' >> pEquation
  return (e:el)

pEquation :: Parsec String () (Term, Term)
pEquation = do
  pTSpace
  t1 <- pTerm
  pTSpace
  char '='
  t2 <- pTerm
  pTSpace
  return (t1, t2)

pArgs :: Parsec String () [Term]
pArgs = do
  a  <- try pTVar <|> pTFunc
  al <- many $ try (pTSpace >> char ',' >> pTSpace >> pTVar) <|>
               try (pTSpace >> char ',' >> pTSpace >> pTFunc)
  return (a:al)
         
pTerm :: Parsec String () Term
pTerm = pTSpace >> (try pTVar <|> pTFunc)

pTFunc :: Parsec String () Term
pTFunc = do
  name <- pTFName
  pTSpace >> char '(' >> pTSpace
  args <- pArgs
  pTSpace >> char ')' >> pTSpace          
  return $ Func name args
  
pTFName :: Parsec String () String
pTFName = do
    pTSpace
    x  <- oneOf $ ['a' .. 'h']
    xs <- many $ oneOf $ '\'':['a' .. 'z'] ++ ['0' .. '9']
    return $ x:xs

pTVar :: Parsec String () Term
pTVar = do
    pTSpace
    x  <- oneOf $ ['i' .. 'z']
    xs <- many $ oneOf $ '\'':['a' .. 'z'] ++ ['0' .. '9']
    return $ Var $ x:xs           
