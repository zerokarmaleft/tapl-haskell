module Arith.Evaluator (eval1, eval) where

import Arith.Syntax

eval1 :: Term -> Maybe Term
eval1 TermTrue                 = Nothing
eval1 TermFalse                = Nothing
eval1 (TermIf TermTrue  t2 _ ) = Just t2
eval1 (TermIf TermFalse _  t3) = Just t3
eval1 (TermIf p         t2 t3) = 
  case eval1 p of
    Just TermTrue  -> Just $ TermIf TermTrue t2 t3
    Just TermFalse -> Just $ TermIf TermFalse t2 t3
    Just t1        -> Just $ TermIf t1 t2 t3
    Nothing        -> Nothing
eval1 TermZero                 = Nothing
eval1 (TermSucc t)             = 
  case eval1 t of
    Just t' -> Just $ TermSucc t'
    Nothing -> Nothing 
eval1 (TermPred TermZero)      = Just TermZero
eval1 (TermPred (TermSucc t))  = Just t
eval1 (TermPred t)             = 
  case eval1 t of
    Just t' -> Just $ TermPred t'
    Nothing -> Nothing
eval1 (TermIsZero TermZero)    = Just TermTrue
eval1 (TermIsZero (TermSucc TermZero)) = Just TermFalse
eval1 (TermIsZero t)           = 
  case eval1 t of
    Just t' -> Just $ TermIsZero t'
    Nothing -> Nothing

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t
