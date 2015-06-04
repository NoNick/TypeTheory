module Expr (Atom (Variable, Abstraction, Brackets),
             Var, Expr (Apply, Single), fv, substitute) where

import Text.Parsec
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
                        
fv (Apply al)         = concatMap fv' al
fv (Single a)         = fv' a
fv' (Abstraction v e) = delete v (fv e)
fv' (Brackets e)      = fv e
fv' (Variable v)      = [v]
                        
substitute :: Expr -> Var -> Expr -> Either String Expr
substitute = subExpr []

subApply :: [String] -> Expr -> Var -> [Atom] -> Either String [Atom]
subApply dom subst var [] = Right $ []
subApply dom subst var (a:al) =
    do a' <- subAtom dom subst var a
       al'<- subApply dom subst var al
       return $ a':al'
             
subExpr :: [String] -> Expr -> Var -> Expr -> Either String Expr
subExpr dom subst var (Single a) =
    subAtom dom subst var a >>= (return . Single)
subExpr dom subst var (Apply al) =
    subApply dom subst var al >>= (return . Apply)
              
subAtom :: [String] -> Expr -> Var -> Atom -> Either String Atom
subAtom dom subst var (Abstraction v expr) =
    subExpr (v:dom) subst var expr >>= return . (Abstraction v)

subAtom dom subst var (Brackets expr) =
    subExpr dom subst var expr >>= return . Brackets
subAtom dom subst var (Variable v) =
    if v == var then let sameVars = intersect dom $ fv subst in
        if null sameVars then
            return $ Brackets subst
        else
            Left $ "No opportunity to insert variable " ++ (head sameVars)
    else
        return $ Variable v
     
