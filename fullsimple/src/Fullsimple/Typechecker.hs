module Fullsimple.Typechecker where

import Fullsimple.Context
import Fullsimple.Terms
import Fullsimple.Types

data TypeError = IfArmsTypeMismatch
               | IfGuardBoolTypeExpected
               | SuccArgNatTypeExpected
               | PredArgNatTypeExpected
               | IsZeroArgNatTypeExpected
               | ProjProductTypeExpected
               | ArrowParamTypeMismatch
               | AppOpArrowTypeExpected
               | VarTypeErrorWat
               deriving (Eq, Show)

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TermTrue                   = Right TypeBool
typeOf _ TermFalse                  = Right TypeBool
typeOf ctx (TermIf t1 t2 t3)
  | typeOf ctx t1 == Right TypeBool =
      if typeOf ctx t2 == typeOf ctx t3
          then typeOf ctx t2
          else Left IfArmsTypeMismatch
  | otherwise                       = Left IfGuardBoolTypeExpected
typeOf _ TermZero                   = Right TypeNat
typeOf ctx (TermSucc t1)
  | typeOf ctx t1 == Right TypeNat  = Right TypeNat
  | otherwise                       = Left SuccArgNatTypeExpected
typeOf ctx (TermPred t1)
  | typeOf ctx t1 == Right TypeNat  = Right TypeNat
  | otherwise                       = Left PredArgNatTypeExpected
typeOf ctx (TermIsZero t1)
  | typeOf ctx t1 == Right TypeNat  = Right TypeBool
  | otherwise                       = Left IsZeroArgNatTypeExpected
typeOf ctx (TermPair t1 t2)         =
  let tyT1 = typeOf ctx t1
      tyT2 = typeOf ctx t2
  in  case tyT1 of
        Right tyT1' ->
          case tyT2 of
            Right tyT2'  -> Right $ TypeProduct tyT1' tyT2'
            Left tyErrT2 -> Left tyErrT2
        Left tyErrT1 -> Left tyErrT1
typeOf ctx (TermProj1 t1)           =
  let tyT1 = typeOf ctx t1
  in  case tyT1 of
        Right (TypeProduct tyT11 tyT12) -> Right tyT11
        Right _                         -> Left ProjProductTypeExpected
        Left tyErrT1                    -> Left tyErrT1
typeOf ctx (TermProj2 t1)           =
  let tyT1 = typeOf ctx t1
  in  case tyT1 of
        Right (TypeProduct tyT11 tyT12) -> Right tyT12
        Right _                         -> Left ProjProductTypeExpected
        Left tyErrT1                    -> Left tyErrT1
typeOf ctx (TermVar x _)            =
  case getType x ctx of
    Just (VarBinding tyT) -> Right tyT
    _                     -> Left VarTypeErrorWat
typeOf ctx (TermAbs x tyT1 t2)      =
  let ctx' = addBinding (x,VarBinding tyT1) ctx
      tyT2 = typeOf ctx' t2
  in  case tyT2 of
        Right tyT2'  -> Right $ TypeArrow tyT1 tyT2'
        Left tyErrT2 -> Left tyErrT2
typeOf ctx (TermApp t1 t2)          =
  let tyT1 = typeOf ctx t1
      tyT2 = typeOf ctx t2
  in case tyT1 of
       Right (TypeArrow tyT11 tyT12) ->
         case tyT2 of
           Right tyT2'  -> if tyT2' == tyT11
                              then Right tyT12
                              else Left ArrowParamTypeMismatch
           Left tyErrT2 -> Left tyErrT2
       _ -> Left AppOpArrowTypeExpected

