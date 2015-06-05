module Parser (pExpr, pVar) where

import Text.Parsec
import Expr
    
pSpace = many $ oneOf " \t\n"
         
pVar :: Parsec String () Var
pVar = do
    x  <- oneOf $ ['a' .. 'z']
    xs <- many $ oneOf $ '\'':['a' .. 'z'] ++ ['0' .. '9']
    return $ x:xs

pAtom :: Parsec String () (Atom Var)
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
        return $ Brackets l)
    
pExpr :: Parsec String () (Expr Var)
pExpr =
    (try pApply >>= (return . Apply)) <|>
    (pAtom >>= (return . Single))
    
pApply :: Parsec String () [Atom Var]
pApply = do
    a  <- pAtom
    as <- many1 $ try $ (many1 $ oneOf " \t\n") >> pAtom
    return $ a:as
