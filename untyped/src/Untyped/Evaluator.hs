module Untyped.Evaluator where

import Untyped.Context
import Untyped.Syntax

-- Variable Shifting and Substitution
--

shiftTerm :: Int -> Term -> Term
shiftTerm d t = walk 0 t
  where walk c t =
          case t of
            TermVar x n   -> 
              if x >= c
                then TermVar (x+d) (n+d)
                else TermVar x     (n+d)
            TermAbs x t1  ->
              TermAbs x (walk (c+1) t1)
            TermApp t1 t2 ->
              TermApp (walk c t1) (walk c t2)

substTerm :: Int -> Term -> Term -> Term
substTerm j s t = walk 0 t
  where walk c t =
          case t of
            TermVar x n -> 
              if x == j+c
                 then shiftTerm c s
                 else TermVar x n
            TermAbs x t1 ->
              TermAbs x (walk (c+1) t1)
            TermApp t1 t2 ->
              TermApp (walk c t1) (walk c t2)

substTopTerm :: Term -> Term -> Term
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation
--

isValue :: Term -> Bool
isValue (TermAbs _ _) = True
isValue _             = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t =
  case t of
    TermApp (TermAbs _ t12) v2 -> 
      if isValue v2
        then Just $ substTopTerm v2 t12
        else Nothing
    TermApp t1 t2              ->
      if isValue t1
         then let t2' = eval1 ctx t2
              in  case t2' of
                    Just t2'' -> Just $ TermApp t1 t2''
                    Nothing   -> Nothing
         else let t1' = eval1 ctx t1
              in  case t1' of
                    Just t1'' -> Just $ TermApp t1'' t2
                    Nothing   -> Nothing
    _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t =
  case eval1 ctx t of
    Just t' -> eval ctx t'
    Nothing -> t
