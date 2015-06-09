module Main where

import Text.Parsec
import Data.List
import Type
import Expr
import Term
import Parser

showType (Var v)           = v
showType (Func _ (a:b:[])) = "(" ++ (showType a) ++ " -> " ++ (showType b) ++ ")"

main = do
  str <- readFile "task6.in"
  case parse pExpr "" str of
    Right e -> case inferType e of
                 Just e' -> writeFile "task6.out" $ t ++ "\n" ++ cnt
                     where t = showType $ snd $ head e'
                           cnt = intercalate "\n" $ map (\(a, b) -> a ++ ":" ++ (showType b)) (tail e')
                 Nothing -> writeFile "task6.out" "The expression has no type"
    Left err   -> writeFile "task6.out" $ show err
