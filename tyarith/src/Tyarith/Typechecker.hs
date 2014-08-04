module Tyarith.Typechecker where

import Arith.Evaluator
import Arith.Parser
import Arith.Syntax

data Type = TypeBool
          | TypeNat
          deriving (Eq, Show)

data TypeError = IfGuardNotBool
               | IfArmsNotSameType
               | SuccArgNotNat
               | PredArgNotNat
               | IsZeroArgNotNat
               deriving (Eq, Show)

typeOf :: Term -> Either TypeError Type
typeOf TermTrue  = Right Bool
typeOf TermFalse = Right Bool
typeOf (TermIf t1 t2 t3) =
  if typeOf t1 == Right Bool
     then if typeOf t2 == typeOf t3
             then typeOf t2
             else Left IfArmsNotSameType
     else Left IfGuardNotBool
typeOf TermZero = Right Nat
typeOf (TermSucc t1) =
  if typeOf t1 == Right Nat
     then Right Nat
     else Left SuccArgNotNat
typeOf (TermPred t1) =
  if typeOf t1 == Right Nat
     then Right Nat
     else Left PredArgNotNat
typeOf (TermIsZero t1) =
  if typeOf t1 == Right Nat
     then Right Bool
     else Left IsZeroArgNotNat
