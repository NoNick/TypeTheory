module Main where

import Data.List
import Text.Parsec
import Term
import Parser

--undef :: [(Term, Term)] -> [(Term, Term)] -> [String]
--undef source solved = (vars' source') \\ (vars' solved')
--      where vars' = nub . concat . (map vars)
--            source' = concat $ map (\(a, b) -> a:[b]) source
--            solved' = map fst source
undef = nub . concat . (map vars) . (map snd)
        
main = do
  str <- readFile "task5.in"
  case parse pSystem "" str of
    Right eqs' -> do
             case unify eqs' of
               Just slv -> do
                    let ans = map (\(a, b) -> (show a) ++ " = " ++ (show b)) slv
                    let und = map (\x -> x ++ " = " ++ x) (undef slv)
                    let ans' = intercalate "\n" (ans ++ und)
                    writeFile "task5.out" ans'
               Nothing -> writeFile "task5.out" "The system has no solution."
    Left err   -> writeFile "task5.out" $ show err
