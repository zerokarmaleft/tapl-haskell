module Untyped.Context where

type Context = [String]

mkContext :: Context
mkContext = []

bindVarName :: Context -> String -> Context
bindVarName ctx x = x : ctx

getVarName :: Context -> Int -> String
getVarName ctx n =
  if length ctx > n
     then ctx !! n
     else error $ ("Requested index " ++ show n
                   ++ " of Context of length " ++ show (length ctx))
                   
freshVarName :: Context -> String -> (Context, String)
freshVarName ctx x =
  let x' = getFreshName ctx x
  in  (bindVarName ctx x', x')
  
getFreshName :: Context -> String -> String
getFreshName [] x = x
getFreshName ctx@(b:bs) x
  | x == b    = getFreshName ctx (x ++ "'")
  | otherwise = getFreshName bs x

ctx1 :: Context
ctx1 = ["b", "a", "z", "y", "x"]
