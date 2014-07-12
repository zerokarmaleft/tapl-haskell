module Simplebool.Typechecker where

import Simplebool.Context
import Simplebool.Syntax

data TypeError = IfArmsTypeMismatch
               | IfGuardNotBool
               | ArrowParamTypeMismatch
               | AppArrowTypeExpected
               | VarTypeErrorWat
               deriving (Eq, Show)

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TermTrue              = Right TypeBool
typeOf _ TermFalse             = Right TypeBool
typeOf ctx (TermIf t1 t2 t3)   =
  if typeOf ctx t1 == Right TypeBool
     then if typeOf ctx t2 == typeOf ctx t3
             then typeOf ctx t2
             else Left IfArmsTypeMismatch
     else Left IfGuardNotBool
typeOf ctx (TermVar x _)       = 
  case getType x ctx of
    Just (VarBinding tyT) -> Right tyT
    _                     -> Left VarTypeErrorWat
typeOf ctx (TermAbs x tyT1 t2) =
  let ctx' = addBinding (x,VarBinding tyT1) ctx
      tyT2 = typeOf ctx' t2
  in case tyT2 of
       Right tyT2' -> Right $ TypeArrow tyT1 tyT2'
       Left tyErrT2 -> Left tyErrT2
typeOf ctx (TermApp t1 t2)     =
  let tyT1 = typeOf ctx t1
      tyT2 = typeOf ctx t2
  in  case tyT1 of
        Right (TypeArrow tyT11 tyT12) -> 
          if tyT2 == Right tyT11
             then Right tyT12
             else Left ArrowParamTypeMismatch
        _ -> Left AppArrowTypeExpected
