module DeBrujin (DeVar, toDe, fromDe) where

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
varList =  varList' "a"

deFV :: Expr DeVar -> [DeVar]
deFV (Expr al)          = nub $ filter (>= 0) $ concatMap deFV' al
deFV' (Variable v)      = [v]
deFV' (Abstraction _ e) = map pred $ deFV e
deFV' (Brackets e)      = deFV e
                          
fvMap :: Expr Var -> Map.Map Var DeVar
fvMap expr = Map.fromList $ zip (Set.toList $ fv expr) [0, 1 ..]
fvMap' :: Expr DeVar -> Map.Map DeVar Var
fvMap' expr = Map.fromList $ zip (deFV expr) varList

toDe :: Expr Var -> Expr DeVar
toDe e = Expr $ toDe'' e 0 (fvMap e) Map.empty
             
-- args: source application, depth, map of FV variables, map of DOM variables
toDe' :: [Atom Var] -> Int -> Map.Map Var DeVar -> Map.Map Var DeVar -> [Atom DeVar]
toDe' [] d fv dom     = []
toDe' (a:al) d fv dom = cur:(toDe' al d fv dom)
    where cur = toDeAtom a d fv dom

toDe'' (Expr al) = toDe' al
                
-- args: source atom, depth, map of FV variables, map of DOM variables
toDeAtom :: Atom Var -> Int -> Map.Map Var DeVar -> Map.Map Var DeVar -> Atom DeVar
toDeAtom (Variable v) d fv dom = case Map.lookup v dom of
                                 Just i -> Variable i
                                 Nothing -> case Map.lookup v fv of
                                              Just i -> Variable $ i + d
                                              Nothing -> Variable (-1)
toDeAtom (Abstraction v e) d fv dom = Abstraction 0 (Expr e')
    where dom' = Map.insert v 0 $ Map.map succ dom
          e' = toDe'' e (succ d) fv dom'
                                            
toDeAtom (Brackets e) d fv dom = Brackets $ Expr $ toDe'' e d fv dom

fromDe :: Expr DeVar -> Expr Var
fromDe e = Expr $ fromDe'' e 0 (varList !! fvLen) (fvMap' e) Map.empty
    where fvLen = length $ deFV $ e
           
-- args: source application, depth, last name, map of variables
-- return: new atom, new last name, new map of variables
fromDe' :: [Atom DeVar] -> Int -> Var -> Map.Map DeVar Var -> Map.Map DeVar Var -> [Atom Var]
fromDe' [] d l fv dom     = []           
fromDe' (a:al) d l fv dom = cur:(fromDe' al d l fv dom)
    where cur = fromDeAtom a d l fv dom
                                                   
fromDe'' (Expr al) = fromDe' al
                     
-- args: source atom, depth, last name, map of variables
-- return: new atom, new last name, new map of variables
fromDeAtom :: Atom DeVar -> Int -> Var -> Map.Map DeVar Var -> Map.Map DeVar Var -> Atom Var
fromDeAtom (Variable v) d l fv dom = case Map.lookup v fv of
                                       Just str -> Variable str
                                       Nothing -> case Map.lookup v dom of
                                                    Just str -> Variable str
                                                    Nothing -> Variable "-1"
fromDeAtom (Abstraction v e) d l fv dom = Abstraction l $ Expr $ fromDe'' e (succ d) l' fv' dom'
    where l' = succVar l
          dom' = Map.insert 0 l $ Map.mapKeys succ dom
          fv' = Map.mapKeys succ fv
                 
fromDeAtom (Brackets e) d l fv dom = Brackets $ Expr $ fromDe'' e d l fv dom
