module Fullsimple.Types where

data Type = TypeBool
          | TypeNat
          | TypeArrow Type Type
          deriving (Eq)

instance Show Type where
  show TypeBool              = "Bool"
  show TypeNat               = "Nat"
  show (TypeArrow tyT1 tyT2) = show tyT1 ++ "->" ++ show tyT2
