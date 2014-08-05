module Fullsimple.Syntax where

import Fullsimple.Context
import Fullsimple.Types
import Control.Monad
import Data.Maybe

data Term = TermTrue
          | TermFalse
          | TermIf     Term Term Term
          | TermZero
          | TermSucc   Term
          | TermPred   Term
          | TermIsZero Term
          | TermVar    Int Int
          | TermAbs    String Type Term
          | TermApp    Term Term
          deriving (Eq, Show)

foldNat :: Term -> Maybe Int
foldNat TermZero     = return 0
foldNat (TermSucc n) = liftM2 (+) (return   1)  (foldNat n)
foldNat (TermPred n) = liftM2 (+) (return (-1)) (foldNat n)
foldNat _            = Nothing

showTerm :: Context -> Term -> String
showTerm ctx TermTrue           = "true"
showTerm ctx TermFalse          = "false"
showTerm ctx (TermIf t1 t2 t3)  = "(if " ++ showTerm ctx t1 ++ " then " ++ showTerm ctx t2 ++ " else " ++ showTerm ctx t3 ++ ")"
showTerm ctx TermZero = "0"
showTerm ctx t@(TermSucc t1)    =
  let n = foldNat t
  in  fromMaybe ("(succ " ++ showTerm ctx t1 ++ ")") (liftM show n)
showTerm ctx t@(TermPred t1)    =
  let n = foldNat t
  in  fromMaybe ("(pred " ++ showTerm ctx t1 ++ ")") (liftM show n)
showTerm ctx (TermIsZero t)     = "(zero? " ++ showTerm ctx t ++ ")"
showTerm ctx (TermVar n _)      = fromMaybe "<undefined var>" (getName n ctx)
showTerm ctx (TermAbs x tyX t1) = 
  let (x', ctx') = freshVarName x ctx
  in  "(lambda " ++ x' ++ ":" ++ show tyX ++ "." ++ showTerm ctx' t1 ++ ")"
showTerm ctx (TermApp t1 t2)    = "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"
