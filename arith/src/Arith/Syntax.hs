module Arith.Syntax where

data Term = TermTrue
          | TermFalse
          | TermIf Term Term Term
          | TermZero
          | TermSucc Term
          | TermPred Term
          | TermIsZero Term
          deriving (Show) 
