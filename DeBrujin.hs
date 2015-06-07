module DeBrujin (DeVar, toDe, fromDe, deReduce) where

import Expr
import Data.Char
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
    
type DeVar = Int

succVar :: Var -> Var
succVar [] = "a"
succVar v
    | (last v) < 'z' = (init v) ++ [succ $ last v]
    | otherwise = succVar (init v) ++ "a"
varList' x = x:(varList' $ succVar x)
varList    =  varList' "a"

deFV :: Expr DeVar -> [DeVar]
deFV (Apply al)        = nub $ filter (>= 0) $ concatMap deFV al
deFV (Variable v)      = [v]
deFV (Abstraction _ e) = map pred $ deFV e
                          
fvMap :: Expr Var -> Map.Map Var DeVar
fvMap expr = Map.fromList $ zip (Set.toList $ fv expr) [0, 1 ..]
fvMap' :: Expr DeVar -> Map.Map DeVar Var
fvMap' expr = Map.fromList $ zip (deFV expr) varList

toDe :: Expr Var -> Expr DeVar
toDe e = toDe' e 0 (fvMap e) Map.empty
             
-- args: source application, depth, map of FV variables, map of DOM variables
toDe' :: Expr Var -> Int -> Map.Map Var DeVar -> Map.Map Var DeVar -> Expr DeVar
toDe' (Apply []) d fv dom     = Apply []
toDe' (Apply (a:al)) d fv dom = Apply (a':al')
    where a'        = toDe' a d fv dom
          -- apply conversion always returns apply
          Apply al' = toDe' (Apply al) d fv dom 

toDe' (Variable v) d fv dom = case Map.lookup v dom of
                                Just i -> Variable i
                                Nothing -> case Map.lookup v fv of
                                             Just i -> Variable $ i + d
                                             Nothing -> Variable (-1)
toDe' (Abstraction v e) d fv dom = Abstraction 0 e'
    where dom' = Map.insert v 0 $ Map.map succ dom
          e' = toDe' e (succ d) fv dom'
                                            
fromDe :: Expr DeVar -> Expr Var
fromDe e = fromDe' e 0 (varList !! fvLen) (fvMap' e) Map.empty
    where fvLen = length $ deFV $ e
           
-- args: source expression, depth, last name, map of variables
fromDe' :: Expr DeVar -> Int -> Var -> Map.Map DeVar Var -> Map.Map DeVar Var -> Expr Var
fromDe' (Apply []) d l fv dom     = Apply []           
fromDe' (Apply (a:al)) d l fv dom = Apply (a':al')
    where a'        = fromDe' a d l fv dom
          Apply al' = fromDe' (Apply al) d l fv dom
fromDe' (Variable v) d l fv dom = case Map.lookup v fv of
                                    Just str -> Variable str
                                    Nothing -> case Map.lookup v dom of
                                                 Just str -> Variable str
                                                 Nothing -> Variable "-1"
fromDe' (Abstraction v e) d l fv dom = Abstraction l e'
    where e' =  fromDe' e (succ d) (succVar l) fv' dom'
          dom' = Map.insert 0 l $ Map.mapKeys succ dom
          fv' = Map.mapKeys succ fv
                 
-- args: shift method, depth, expression
deShift :: (Int -> Int) -> Int -> Expr DeVar -> Expr DeVar
deShift f d (Apply al)        = Apply $ map (deShift f d) al
deShift f d (Variable v)      = if v < d then Variable v
                                else Variable $ succ v
deShift f d (Abstraction v e) = Abstraction v $ deShift f (f d) e

-- subtitutes number v for v' in expression (works for dom vars)
deSubst :: DeVar -> Expr DeVar -> Expr DeVar -> Expr DeVar
deSubst v v' (Apply al)          = Apply $ map (deSubst v v') al
deSubst v v' (Variable var)      = case compare var v of
                                     EQ -> v'
                                     GT -> Variable $ pred var
                                     LT -> Variable var
deSubst v v' (Abstraction var e) = if (succ v) == var then e''
                                   else Abstraction var e'
                                     where e' = deSubst (succ v) (deShift succ 0 v') e
                                           e'' = deSubst (succ v) v' e

--deNormalize :: Expr DeVar -> Expr DeVar
--deNormalize (Variable var)    = Variable var               
--deNormalize (Apply (a:[]))    = deNormalize a
--deNormalize (Apply al)        = Apply $ map deNormalize al
--deNormalize (Abstraction v e) = Abstraction v $ deNormalize e
                             
deReduce :: Expr DeVar -> Expr DeVar
deReduce (Apply (a:[]))    = deReduce a
deReduce (Apply (a:al))    = case deReduce a of
                              Abstraction v e -> deReduce $ deSubst 0 (Apply al) e 
                              a' -> case deReduce $ Apply al of
                                      Apply al' -> Apply $ a':al'
                                      smth      -> Apply $ a':[smth]
deReduce (Variable var)    = Variable var
deReduce (Abstraction v e) = Abstraction v $ deReduce e
