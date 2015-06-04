module Main where
    
import Text.Parsec
import System.IO
import Expr
import Parser

main = do
  str <- readFile "task1.in"
  let result = parse pExpr "" str
  let answer = case result of
                 Right parsed -> show parsed
                 Left  error  -> show error
  writeFile "task1.out" $ answer ++ "\n"

  
