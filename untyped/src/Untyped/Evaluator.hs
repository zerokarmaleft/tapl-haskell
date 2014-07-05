module Untyped.Evaluator where

import Untyped.Syntax

-- Variable Shifting and Substitution
--

shiftTerm :: Int -> Term -> Term
shiftTerm d t = walk 0 t
  where walk c (TermVar x n)
          | n >= c             = TermVar (x+d) (n+d)
          | otherwise          = TermVar (x+1) n
        walk c (TermAbs x t1)  = TermAbs x (walk (c+1) t1)
        walk c (TermApp t1 t2) = TermApp (walk c t1) (walk c t2)

substTerm :: Int -> Term -> Term -> Term
substTerm j s t = walk 0 t
  where walk c (TermVar x n)
          | n == j+c           = shiftTerm c s
          | otherwise          = TermVar x n
        walk c (TermAbs x t1)  = TermAbs x (walk (c+1) t1)
        walk c (TermApp t1 t2) = TermApp (walk c t1) (walk c t2)

substTopTerm :: Term -> Term -> Term
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation
--

isValue :: Term -> Bool
isValue (TermAbs _ _) = True
isValue _             = False

eval1 :: Term -> Maybe Term
eval1 (TermApp (TermAbs _ t12) v2)
  | isValue v2 = Just $ substTopTerm v2 t12
  | otherwise  = Nothing
eval1 (TermApp t1 t2)
  | isValue t1 = let t2' = eval1 t2
                 in  case t2' of
                     Just t2'' -> Just $ TermApp t1 t2''
                     Nothing   -> Nothing
  | otherwise = let t1' = eval1 t1
                in  case t1' of
                      Just t1'' -> Just $ TermApp t1'' t2
                      Nothing   -> Nothing
eval1 _ = Nothing

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t
