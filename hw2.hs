module Main where

import qualified Data.Set as Set
import Text.Parsec
import Data.List
import System.IO
import Expr
import Parser

main = do
  str <- readFile "task2.in"
  let parsed = parse pExpr "" str
  let free = case parsed of
               Right expr   -> intercalate "\n" $ sort $ Set.toList $ fv expr
               Left  error  -> show error
  writeFile "task2.out" $ free ++ "\n"

