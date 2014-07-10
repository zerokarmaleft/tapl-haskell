module Untyped.Context where

type Context = [String]

mkContext :: Context
mkContext = []

bindVarName :: String -> Context -> Context
bindVarName = (:)

getVarName :: Int -> Context -> String
getVarName n ctx
  | length ctx > n = ctx !! n
  | otherwise      = error $ ("Requested index " ++ show n
                              ++ " of Context of length " ++ show (length ctx))

freshVarName :: String -> Context -> (String, Context)
freshVarName x ctx =
  let x' = mkFreshVarName x ctx
  in  (x', bindVarName x' ctx)

mkFreshVarName :: String -> Context -> String
mkFreshVarName x [] = x
mkFreshVarName x ctx@(b:bs)
  | x == b    = mkFreshVarName (x ++ "'") ctx
  | otherwise = mkFreshVarName x bs

