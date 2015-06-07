module Parser (pExpr, pVar) where

import Text.Parsec
import Expr
    
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
