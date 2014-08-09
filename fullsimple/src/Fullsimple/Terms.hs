module Fullsimple.Terms where

import Fullsimple.Types

data Term = TermUnit
          | TermAscription Term Type
          | TermTrue
          | TermFalse
          | TermIf         Term Term Term
          | TermZero
          | TermSucc       Term
          | TermPred       Term
          | TermIsZero     Term
          | TermProduct    [Term]
          | TermProj       Int Term
          | TermVar        Int Int
          | TermAbs        String Type Term
          | TermApp        Term Term
          deriving (Eq, Show)

