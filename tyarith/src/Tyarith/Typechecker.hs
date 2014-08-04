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
typeOf TermTrue                 = Right TypeBool
typeOf TermFalse                = Right TypeBool
typeOf (TermIf t1 t2 t3)
  | typeOf t1 == Right TypeBool = if typeOf t2 == typeOf t3
                                     then typeOf t2
                                     else Left IfArmsNotSameType
  | otherwise                   = Left IfGuardNotBool
typeOf TermZero                 = Right TypeNat
typeOf (TermSucc t1)
  | typeOf t1 == Right TypeNat  = Right TypeNat
  | otherwise                   = Left SuccArgNotNat
typeOf (TermPred t1)
  | typeOf t1 == Right TypeNat  = Right TypeNat
  | otherwise                   = Left PredArgNotNat
typeOf (TermIsZero t1)
  | typeOf t1 == Right TypeNat  = Right TypeNat
  | otherwise                   = Left IsZeroArgNotNat

