module Fullsimple.Terms where

import Fullsimple.Types

data Term = TermTrue
          | TermFalse
          | TermIf      Term Term Term
          | TermZero
          | TermSucc    Term
          | TermPred    Term
          | TermIsZero  Term
          | TermPair    Term Term
          | TermProj1   Term
          | TermProj2   Term
          | TermProduct [Term]
          | TermProj    Int Term
          | TermVar     Int Int
          | TermAbs     String Type Term
          | TermApp     Term Term
          deriving (Eq, Show)

