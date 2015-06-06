module Expr (Atom (Variable, Abstraction, Brackets),
             Var, Expr (Expr), fv, substitute) where

import Text.Parsec
import Data.List
import qualified Data.Set as Set
    
type Var = String
data Atom a = Variable a | Abstraction a (Expr a) | Brackets (Expr a)
newtype Expr a = Expr [(Atom a)]

instance Show a => Show (Expr a) where
    show (Expr (a:[]) ) = show a
    show (Expr al)      = ob ++ first ++ " " ++ rest ++ ")"
            where ob = replicate ((length al) - 1) '('
                  first = show $ head al
                  rest = intercalate ") " $ map show $ tail al

instance Show a => Show (Atom a) where
    show (Variable var) = show' var
    show (Abstraction v l) = "(\\" ++ (show' v) ++ "." ++ (show l) ++ ")"
    show (Brackets l) = show l

show' a = if (head str) == '"' && (last str) == '"' then
              init $ tail str
          else
              str
              where str = show a
    
fv  (Expr al)         = concatMap fv' al
fv' (Abstraction v e) = delete v (fv e)
fv' (Brackets e)      = fv e
fv' (Variable v)      = [v]
                        
substitute :: (Ord a, Show a) => Expr a -> a -> Expr a -> Either String (Expr a)
substitute subst = subExpr (Set.fromList $ fv subst) Set.empty subst

subApply :: (Ord a, Show a) => Set.Set a -> Set.Set a -> Expr a ->
            a -> [Atom a] -> Either String [Atom a]
subApply nfv dom subst var [] = Right $ []
subApply nfv dom subst var (a:al) =
    do a' <- subAtom nfv dom subst var a
       al'<- subApply nfv dom subst var al
       return $ a':al'
             
subExpr :: (Ord a, Show a) => Set.Set a -> Set.Set a -> Expr a ->
           a -> Expr a -> Either String (Expr a)
subExpr nfv dom subst var (Expr al) =
    subApply nfv dom subst var al >>= (return . Expr)
              
subAtom :: (Ord a, Show a) => Set.Set a -> Set.Set a -> Expr a ->
           a -> Atom a -> Either String (Atom a)
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
                     (show' $ head $ Set.elems sameVars)
    else
        return $ Variable v
     
