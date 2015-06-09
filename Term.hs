module Term (Term (Var, Func), unify, vars) where

import Data.List
import Data.Tuple
    
data Term = Var String | Func String [Term]

instance Show Term where
    show (Var var) = var
    show (Func fname args) = fname ++ "(" ++ args' ++ ")"
        where args' = intercalate ", " $ map show args

instance Eq Term where
    (==) (Var v1) (Var v2)         = v1 == v2
    (==) (Func f1 a1) (Func f2 a2) = (f1 == f2) && (and $ map (\(a, b) -> a == b) a')
                                     where a' = zip a1 a2
    (==) _ _                       = False
                                     
vars (Var str) = [str]
vars (Func name terms) = nub $ concat $ map vars terms
                         
-- Substitute v fot t in src
subst:: String -> Term -> Term -> Term
subst v t src@(Var var)
    | v == var  = t
    | otherwise = src
subst v t (Func name args) = Func name $ map (subst v t) args
      
unify :: [(Term, Term)] -> Maybe [(Term, Term)]
unify eqs = unify' eqs 0

unify' :: [(Term, Term)] -> Int -> Maybe [(Term, Term)]
unify' [] _ = Just []
unify' (e@(t1, t2):el) n
    | t1 == t2    = unify' el 0
unify' (e@(Var v, t):el) n
    | (not (elem v $ vars t)) && contain = unify' (e:el') 0
      where contain = or $ map (\(a, b) -> elem v $ (vars a) ++ (vars b)) el
            el' = map (\(a, b) -> (subst v t a, subst v t b)) el
unify' (e@((Func _ _), (Var _)):el) n = unify' ((swap e):el) 0
unify' (e@((Func f1 args1), (Func f2 args2)):el) n
    | (f1 /= f2) || ((length args1) /= (length args2)) = Nothing
    | otherwise = unify' (el ++ (zip args1 args2)) 0
unify' (e@((Var v), f@(Func _ _)):el) n
    | elem v (vars f) = Nothing
unify' eqs@(e:el) n
    | (length eqs) < n = Just eqs
    | otherwise        = unify' (el ++ [e]) (succ n)
