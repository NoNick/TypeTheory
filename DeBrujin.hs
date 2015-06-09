module DeBrujin (DeVar, toDe, fromDe, deReduce,
                 succVar, varList) where

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
deFV (Abstraction _ e) = filter (>= 0) $ map pred $ deFV e
                          
fvMap :: Expr Var -> Map.Map Var DeVar
fvMap expr = Map.fromList $ zip (Set.toList $ fv expr) [0, 1 ..]
fvMap' :: Expr DeVar -> Map.Map DeVar Var
fvMap' expr = Map.fromList $ zip (deFV expr) varList

toDe :: Expr Var -> Expr DeVar
toDe e = toDe' 0 (fvMap e) Map.empty e
             
-- args: depth, map of FV variables, map of DOM variables, source application
toDe' :: Int -> Map.Map Var DeVar -> Map.Map Var DeVar -> Expr Var -> Expr DeVar
toDe' d fv dom (Apply al) = Apply $ map (toDe' d fv dom) al
toDe' d fv dom (Variable v) = case Map.lookup v dom of
                                Just i -> Variable i
                                Nothing -> case Map.lookup v fv of
                                             Just i -> Variable $ i + d
                                             Nothing -> Variable (-1)
toDe' d fv dom (Abstraction v e) = Abstraction 0 e'
    where dom' = Map.insert v 0 $ Map.map succ dom
          e' = toDe' (succ d) fv dom' e
                                            
fromDe :: Expr DeVar -> Expr Var
fromDe e = fromDe' 0 (varList !! fvLen) (fvMap' e) Map.empty e
    where fvLen = length $ deFV $ e
           
-- args: depth, last name, map of variables, source expression
fromDe' :: Int -> Var -> Map.Map DeVar Var -> Map.Map DeVar Var -> Expr DeVar -> Expr Var
fromDe' d l fv dom (Apply al) = Apply $ map (fromDe' d l fv dom) al
fromDe' d l fv dom (Variable v) = case Map.lookup v fv of
                                    Just str -> Variable str
                                    Nothing -> case Map.lookup v dom of
                                                 Just str -> Variable str
                                                 Nothing -> Variable "-1"
fromDe' d l fv dom (Abstraction v e) = Abstraction l e'
    where e' =  fromDe' (succ d) (succVar l) fv' dom' e
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

deReduce :: Expr DeVar -> Expr DeVar
deReduce (Apply [])        = Apply []
deReduce (Apply (a:[]))    = deReduce a
deReduce (Apply (a:b:[]))  = case deReduce a of
                              Abstraction v e -> deReduce $ (deSubst 0 b e)
                              a' -> Apply (a':[deReduce b])
deReduce (Apply (a:al))    = case deReduce a of
                              Abstraction v e -> deReduce $ Apply $ (deSubst 0 (head al) e):(tail al)
                              a' -> case deReduce $ Apply al of
                                      (Apply al') -> Apply (a':al')
                                      smth          -> Apply (a':[smth])
deReduce (Variable var)    = Variable var
deReduce (Abstraction v e) = Abstraction v $ deReduce e
