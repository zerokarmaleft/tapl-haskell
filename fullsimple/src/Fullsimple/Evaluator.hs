module Fullsimple.Evaluator where

import Control.Monad
import Data.List
import Fullsimple.Terms

-- Variable Shifting and Substitution
--

shiftTerm :: Int -> Term -> Term
shiftTerm d = walk 0
  where walk c (TermIf t1 t2 t3)   = TermIf (walk c t1) (walk c t2) (walk c t3)
        walk c (TermVar x n)
          | x >= c                 = TermVar (x+d) (n+d)
          | otherwise              = TermVar x     (n+d)
        walk c (TermAbs x tyT1 t1) = TermAbs x tyT1 (walk (c+1) t1)
        walk c (TermApp t1 t2)     = TermApp (walk c t1) (walk c t2)
        walk _ t                   = t

substTerm :: Int -> Term -> Term -> Term
substTerm j s = walk 0
  where walk c (TermIf t1 t2 t3)   = TermIf (walk c t1) (walk c t2) (walk c t3)
        walk c (TermSucc t1)       = TermSucc (walk c t1)
        walk c (TermPred t1)       = TermPred (walk c t1)
        walk c (TermIsZero t1)     = TermIsZero (walk c t1)
        walk c (TermProduct ts)    = TermProduct (map (walk c) ts)
        walk c (TermProj x t1)     = TermProj x (walk c t1)
        walk c (TermVar x n)
          | x == j+c               = s
          | otherwise              = TermVar x n
        walk c (TermAbs x tyT1 t1) = TermAbs x tyT1 (walk (c+1) t1)
        walk c (TermApp t1 t2)     = TermApp (walk c t1) (walk c t2)
        walk _ t1
          | t1 == TermTrue         = t1
          | t1 == TermFalse        = t1
          | t1 == TermZero         = t1
          | otherwise              = s

substTopTerm :: Term -> Term -> Term
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation
--

isValue :: Term -> Bool
isValue (TermAbs _ _ _)          = True
isValue TermTrue                 = True
isValue TermFalse                = True
isValue TermZero                 = True
isValue (TermSucc (TermPred t1)) = False
isValue (TermSucc t1)            = isValue t1
isValue (TermPred (TermSucc t1)) = False
isValue (TermPred t1)            = isValue t1
isValue (TermProduct ts)         = (and . map isValue) ts
isValue _                        = False

eval1 :: Term -> Maybe Term
eval1 (TermIf TermTrue  t2 _ )         = return t2
eval1 (TermIf TermFalse _  t3)         = return t3
eval1 (TermIf t1        t2 t3)         = liftM (\t1' -> TermIf t1' t2 t3) (eval1 t1)
eval1 (TermSucc t1)                    = liftM TermSucc (eval1 t1)
eval1 (TermPred TermZero)              = return TermZero
eval1 (TermPred (TermSucc t1))         = return t1
eval1 (TermPred t1)                    = liftM TermPred (eval1 t1)
eval1 (TermIsZero TermZero)            = return TermTrue
eval1 (TermIsZero (TermSucc TermZero)) = return TermFalse
eval1 t@(TermProduct ts)
  | isValue t                          = Nothing
  | otherwise                          = liftM TermProduct (mapM (\t -> if isValue t then return t else eval1 t) ts)
eval1 (TermProj x (TermProduct ts))    = return $ ts !! x
eval1 (TermProj x t1)                  = liftM2 TermProj (return x) (eval1 t1)
eval1 (TermIsZero t)                   = liftM TermIsZero (eval1 t)
eval1 (TermApp (TermAbs _ _ t12) v2)
  | isValue v2                         = return $ substTopTerm v2 t12
eval1 (TermApp t1 t2)
  | isValue t1                         = liftM2 TermApp (return t1) (eval1  t2)
  | otherwise                          = liftM2 TermApp (eval1  t1) (return t2)
eval1 _                                = Nothing

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

