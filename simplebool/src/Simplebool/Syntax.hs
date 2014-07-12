module Simplebool.Syntax where

import Simplebool.Context

data Term = TermTrue
          | TermFalse
          | TermIf  Term Term Term
          | TermVar Int Int
          | TermAbs String Type Term
          | TermApp Term Term
          deriving (Show)
          
showTerm :: Context -> Term -> String
showTerm ctx t =
  case t of
    TermTrue -> "true"
    TermFalse -> "false"
    TermIf t1 t2 t3 ->
      "(if " ++ showTerm ctx t1 ++ " then " ++ showTerm ctx t2 ++ " else " ++ showTerm ctx t3 ++ ")"
    TermVar n _ -> 
      case getName n ctx of
        Just x -> x
        Nothing -> ""
    TermAbs x _ t1 ->
      let (x', ctx') = freshVarName x ctx
      in  "(lambda " ++ x' ++ "." ++ showTerm ctx' t1 ++ ")"
    TermApp t1 t2 ->
      "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"
