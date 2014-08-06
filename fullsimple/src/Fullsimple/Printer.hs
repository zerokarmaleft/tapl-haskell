module Fullsimple.Printer where

import Fullsimple.Context
import Fullsimple.Terms
import Control.Monad
import Data.Maybe

foldNat :: Term -> Maybe Int
foldNat TermZero     = return 0
foldNat (TermSucc n) = liftM2 (+) (return   1)  (foldNat n)
foldNat (TermPred n) = liftM2 (+) (return (-1)) (foldNat n)
foldNat _            = Nothing

printTerm :: Context -> Term -> String
printTerm ctx TermTrue           = "true"
printTerm ctx TermFalse          = "false"
printTerm ctx (TermIf t1 t2 t3)  = "(if " ++ printTerm ctx t1 ++ " then " ++ printTerm ctx t2 ++ " else " ++ printTerm ctx t3 ++ ")"
printTerm ctx TermZero           = "0"
printTerm ctx t@(TermSucc t1)    =
  let n = foldNat t
  in  fromMaybe ("(succ " ++ printTerm ctx t1 ++ ")") (liftM show n)
printTerm ctx t@(TermPred t1)    =
  let n = foldNat t
  in  fromMaybe ("(pred " ++ printTerm ctx t1 ++ ")") (liftM show n)
printTerm ctx (TermIsZero t)     = "(zero? " ++ printTerm ctx t ++ ")"
printTerm ctx (TermVar n _)      = fromMaybe "<undefined var>" (getName n ctx)
printTerm ctx (TermAbs x tyX t1) = 
  let (x', ctx') = freshVarName x ctx
  in  "(lambda " ++ x' ++ ":" ++ show tyX ++ "." ++ printTerm ctx' t1 ++ ")"
printTerm ctx (TermApp t1 t2)    = "(" ++ printTerm ctx t1 ++ " " ++ printTerm ctx t2 ++ ")"

