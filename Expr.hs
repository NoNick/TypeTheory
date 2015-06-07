module Expr (Expr (Variable, Abstraction, Apply),
             Var, fv, substitute) where

import Text.Parsec
import Data.List
import qualified Data.Set as Set
    
type Var = String
data Expr a = Variable a | Apply [(Expr a)] | Abstraction a (Expr a)

instance Show a => Show (Expr a) where
    show (Variable var) = show' var
    show (Abstraction v l) = "(\\" ++ (show' v) ++ "." ++ (show l) ++ ")"
    show (Apply (a:[]) ) = show a
    show (Apply al)      = ob ++ first ++ " " ++ rest ++ ")"
            where ob = replicate ((length al) - 1) '('
                  first = show $ head al
                  rest = intercalate ") " $ map show $ tail al

show' a = if (head str) == '"' && (last str) == '"' then
              init $ tail str
          else
              str
              where str = show a
    
fv (Apply al)        = Set.unions $ map fv al
fv (Abstraction v e) = Set.delete v (fv e)
fv (Variable v)      = Set.singleton v
                        
substitute :: (Ord a, Show a) => Expr a -> a -> Expr a -> Either String (Expr a)
substitute subst = substitute' (fv subst) Set.empty subst
substitute' :: (Ord a, Show a) => Set.Set a -> Set.Set a -> Expr a -> a -> Expr a -> Either String (Expr a) 
substitute' nfv dom subst var (Apply [])     = Right $ Apply []
substitute' nfv dom subst var (Apply (a:al)) =
    do a' <- substitute' nfv dom subst var a
       Apply al'<- substitute' nfv dom subst var (Apply al)
       return $ Apply (a':al')
substitute' nfv dom subst var (Abstraction v expr) =
    substitute' nfv (Set.insert v dom) subst var expr >>=
            return . (Abstraction v)
substitute' nfv dom subst var (Variable v) =
    if v == var then let sameVars = Set.intersection dom nfv in
        if Set.null sameVars then
            return subst
        else
            Left $ "No opportunity to insert variable " ++
                     (show' $ head $ Set.elems sameVars)
    else
        return $ Variable v
     
