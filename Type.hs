module Type (inferType) where

import Control.Monad.Writer
import Control.Monad.State
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Expr
import Term
import DeBrujin
    
fvMap :: Expr Var -> Map.Map Var Term
fvMap expr = Map.fromList $ zip (Set.toList $ fv expr) (map Var varList)

nextName = modify (\name -> succVar name)
-- args: map of substitutions, expression and its type term
-- returns nothing, writes to system of equations and carry next type name
system :: Map.Map Var Term -> Expr Var -> Term -> StateT String (Writer [(Term, Term)]) ()
system s (Variable v) t      = tell [(t, t')]
    where t' = case Map.lookup v s of
                 Just smth -> smth
                 Nothing -> Var "Error"
system s (Apply (e:[])) t    = system s e t
system s (Apply el) t        = do
  t1 <- get
  nextName
  system s (Apply $ init el) (Func "imply" [Var t1, t])
  system s (last el) (Var t1)
system s (Abstraction v e) t = do
  t1 <- get
  nextName       
  t2 <- get
  nextName
  tell [(t, Func "imply" [Var t1, Var t2])]
  let s' = Map.insert v (Var t1) s
  system s' e (Var t2)

findType :: Term -> [(Term, Term)] -> Term
findType v solved = case findIndex (== v) $ fst $ unzip solved of
                      Just indx -> (snd $ unzip solved) !! indx
                      Nothing   -> v

inferType :: Expr Var -> Maybe [(String, Term)]
inferType expr = do
  let m   = fvMap expr
  let v   = varList !! (Map.size m)
  let eqs = execWriter $ runStateT (system m expr (Var v)) (succVar v)
  solved <- unify eqs
  let t   = findType (Var v) solved
  let cnt = map (\(x, t) -> (x, findType t solved)) (Map.toList m)
  return (("", t):cnt)
