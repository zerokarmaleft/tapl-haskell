module Untyped.Syntax where

import Untyped.Context

data Term = TermVar Int Int
          | TermAbs String Term
          | TermApp Term Term
          deriving (Show)

showTerm :: Context -> Term -> String
showTerm ctx t =
  case t of
    TermVar n _   ->
      getVarName n ctx
    TermAbs x t1  -> 
      let (x', ctx') = freshVarName x ctx
      in  "(lambda " ++ x' ++ "." ++ showTerm ctx' t1 ++ ")"
    TermApp t1 t2 -> 
      "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"

