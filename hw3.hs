module Main where
    
import Text.Parsec
import System.IO
import Expr
import Parser

data Clause = Clause {expr  :: String,
                      var   :: String,
                      subst :: String} deriving Show

pClause :: Parsec String () Clause
pClause = do
  e <- many1 $ noneOf "["
  char '['
  many $ oneOf " \t\n"
  v <- pVar
  many $ oneOf " \t\n"
  string ":="
  s <- many1 $ noneOf "]"
  char ']'
  return $ Clause e v s

err (Right e) = Right e
err (Left pe) = Left $ show pe

doStuff :: Clause -> Either String (Expr Var)
doStuff clause = do
  e <- err $ parse pExpr "" $ expr  clause
  v <- err $ parse pVar  "" $ var   clause
  s <- err $ parse pExpr "" $ subst clause
  substitute s v e
    
main = do
  str <- readFile "task3.in"
  let parsed = parse pClause "" str
  case parsed of
    Left err     -> writeFile "task3.out" $ show err ++ "\n"
    Right clause -> writeFile "task3.out" $ answer ++ "\n"
      where answer = case doStuff clause of
                       Left err   -> err
                       Right expr -> show expr
