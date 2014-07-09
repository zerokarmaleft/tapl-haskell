module Untyped.Evaluator where

import Control.Monad
import Debug.Trace
import Untyped.Syntax

-- Variable Shifting and Substitution
--

shiftTerm :: Int -> Term -> Term
-- shiftTerm d t = walk 0 t
--   where walk c (TermVar x n)
--           | n >= c             = TermVar (x+d) (n+d)
--           | otherwise          = TermVar x     (n+d)
--         walk c (TermAbs x t1)  = TermAbs x (walk (c+1) t1)
--         walk c (TermApp t1 t2) = TermApp (walk c t1) (walk c t2)
shiftTerm d t = trace ("shiftTerm[" ++ show d ++ "]: \t\t" ++ show t) $ walk 0 t
  where walk c (TermVar x n)
          | n >= c             = trace ("shiftWalk[" ++ show (c,d) ++ "]: \t" ++ show (TermVar x n)) $ TermVar (x+d) (n+d)
          | otherwise          = trace ("shiftWalk[" ++ show (c,d) ++ "]: \t" ++ show (TermVar x n)) $ TermVar x    (n+d)
        walk c (TermAbs x t1)  = trace ("shiftWalk[" ++ show (c,d) ++ "]: \t" ++ show (TermAbs x t1)) $ TermAbs x (walk (c+1) t1)
        walk c (TermApp t1 t2) = trace ("shiftWalk[" ++ show (c,d) ++ "]: \t" ++ show (TermApp t1 t2)) $ TermApp (walk c t1) (walk c t2)

substTerm :: Int -> Term -> Term -> Term
-- substTerm j s t = walk 1 t
--   where walk c (TermVar x n)
--           | n == j+c           = shiftTerm c s
--           | otherwise          = TermVar x n
--         walk c (TermAbs x t1)  = TermAbs x (walk (c+1) t1)
--         walk c (TermApp t1 t2) = TermApp (walk c t1) (walk c t2)
substTerm j s t = trace ("substTerm[" ++ show j ++ "]: \t\t" ++ show (s,t)) $ walk 1 t
  where walk c (TermVar x n)
          | n == j+c           = trace ("substWalk[" ++ show (c,j) ++ "]: \t" ++ show (TermVar x n)) $ shiftTerm c s
          | otherwise          = trace ("substWalk[" ++ show (c,j) ++ "]: \t" ++ show (TermVar x n)) $ TermVar x n
        walk c (TermAbs x t1)  = trace ("substWalk[" ++ show (c,j) ++ "]: \t" ++ show (TermAbs x t1)) $ TermAbs x (walk (c+1) t1)
        walk c (TermApp t1 t2) = trace ("substWalk[" ++ show (c,j) ++ "]: \t" ++ show (TermApp t1 t2)) $ TermApp (walk c t1) (walk c t2)

substTopTerm :: Term -> Term -> Term
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation
--

isValue :: Term -> Bool
isValue (TermAbs _ _) = True
isValue _             = False

eval1 :: Term -> Maybe Term
eval1 (TermApp (TermAbs _ t12) v2)
  | isValue v2 = return $ substTopTerm v2 t12
eval1 (TermApp t1 t2)
  | isValue t1 = liftM2 TermApp (return t1) (eval1  t2)
  | otherwise  = liftM2 TermApp (eval1  t1) (return t2)
eval1 _ = Nothing

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

