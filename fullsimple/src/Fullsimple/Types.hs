module Fullsimple.Types where

import Data.List

data Type = TypeBool
          | TypeNat
          | TypeProduct [Type]
          | TypeArrow   Type Type
          deriving (Eq)

instance Show Type where
  show TypeBool                = "Bool"
  show TypeNat                 = "Nat"
  show (TypeProduct tyTs)      = "{" ++ (concat . intersperse ", " . map show) tyTs ++ "}"
  show (TypeArrow tyT1 tyT2)   = show tyT1 ++ "->" ++ show tyT2

