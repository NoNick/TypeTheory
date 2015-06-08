module Main where

import Text.Parsec
import Expr
import Parser
import DeBrujin

main = do
  str <- readFile "task4.in"
  case parse pExpr "" str of
    Right e -> writeFile "task4.out" $ (show $ e') ++ "\n"
        where e' = fromDe $ deReduce $ toDe e
    Left s -> writeFile "task4.out" $ show s
  
