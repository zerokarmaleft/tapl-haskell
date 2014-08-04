module Arith.Evaluator (eval1, eval) where

import Arith.Syntax
import Control.Monad

eval1 :: Term -> Maybe Term
eval1 TermTrue                         = Nothing
eval1 TermFalse                        = Nothing
eval1 (TermIf TermTrue  t2 _ )         = Just t2
eval1 (TermIf TermFalse _  t3)         = Just t3
eval1 (TermIf t1        t2 t3)         = liftM3 TermIf (eval1 t1) (return t2) (return t3)
eval1 TermZero                         = Nothing
eval1 (TermSucc t)                     = liftM TermSucc (eval1 t)
eval1 (TermPred TermZero)              = Just TermZero
eval1 (TermPred (TermSucc t))          = Just t
eval1 (TermPred t)                     = liftM TermPred (eval1 t)
eval1 (TermIsZero TermZero)            = Just TermTrue
eval1 (TermIsZero (TermSucc TermZero)) = Just TermFalse
eval1 (TermIsZero t)                   = liftM TermIsZero (eval1 t)

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t
