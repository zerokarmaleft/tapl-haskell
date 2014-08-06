module Fullsimple.Types where

data Type = TypeBool
          | TypeNat
          | TypePair  Type Type
          | TypeArrow Type Type
          deriving (Eq)

instance Show Type where
  show TypeBool              = "Bool"
  show TypeNat               = "Nat"
  show (TypePair tyT1 tyT2)  = "{" ++ show tyT1 ++ ", " ++ show tyT2 ++ "}"
  show (TypeArrow tyT1 tyT2) = show tyT1 ++ "->" ++ show tyT2

