module Expr (Atom (Variable, Abstraction, Brackets),
             Var, Expr (Apply, Single)) where

import Data.List
    
type Var = String
data Atom = Variable Var | Abstraction Var Expr | Brackets Expr
data Expr = Apply [Atom] | Single Atom

instance Show Expr where
    show (Single a) = show a
    show (Apply al) = openBrackets ++ first ++ " " ++ rest ++ ")"
                     where openBrackets = replicate ((length al) - 1) '('
                           first = show $ head al
                           rest = intercalate ") " $ map show $ tail al

instance Show Atom where
    show (Variable v) = v
    show (Abstraction v l) = "(\\" ++ v ++ "." ++ (show l) ++ ")"
    show (Brackets l) = show l
                        
