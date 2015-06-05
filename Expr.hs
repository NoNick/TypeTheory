module Expr (Atom (Variable, Abstraction, Brackets),
             Var, Expr (Apply, Single), fv, substitute) where

import Text.Parsec
import Data.List
import qualified Data.Set as Set
    
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
substitute subst = subExpr (Set.fromList $ fv subst) Set.empty subst

subApply :: Set.Set String -> Set.Set String -> Expr ->
            Var -> [Atom] -> Either String [Atom]
subApply nfv dom subst var [] = Right $ []
subApply nfv dom subst var (a:al) =
    do a' <- subAtom nfv dom subst var a
       al'<- subApply nfv dom subst var al
       return $ a':al'
             
subExpr :: Set.Set String -> Set.Set String -> Expr ->
           Var -> Expr -> Either String Expr
subExpr nfv dom subst var (Single a) =
    subAtom nfv dom subst var a >>= (return . Single)
subExpr nfv dom subst var (Apply al) =
    subApply nfv dom subst var al >>= (return . Apply)
              
subAtom :: Set.Set String -> Set.Set String -> Expr ->
           Var -> Atom -> Either String Atom
subAtom nfv dom subst var (Abstraction v expr) =
    subExpr nfv (Set.insert v dom) subst var expr >>=
            return . (Abstraction v)

subAtom nfv dom subst var (Brackets expr) =
    subExpr nfv dom subst var expr >>= return . Brackets
subAtom nfv dom subst var (Variable v) =
    if v == var then let sameVars = Set.intersection dom nfv in
        if Set.null sameVars then
            return $ Brackets subst
        else
            Left $ "No opportunity to insert variable " ++
                     (head $ Set.elems sameVars)
    else
        return $ Variable v
     
