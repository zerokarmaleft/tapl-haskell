module Fullsimple.Context where

import Fullsimple.Types
import Control.Monad

type Context = [(String, Binding)]

data Binding = NameBinding
             | VarBinding Type
             deriving (Eq, Show)

mkContext :: Context
mkContext = []

addBinding :: (String, Binding) -> Context -> Context
addBinding = (:)

getBinding :: String -> Context -> Maybe Binding
getBinding = lookup

getIndex :: Int -> Context -> Maybe (String, Binding)
getIndex n ctx
  | length ctx > n = Just $ ctx !! n
  | otherwise      = Nothing

getName :: Int -> Context -> Maybe String
getName n ctx = liftM fst $ getIndex n ctx

getType :: Int -> Context -> Maybe Binding
getType n ctx = liftM snd $ getIndex n ctx

freshVarName :: String -> Context -> (String, Context)
freshVarName x ctx =
  let x' = mkFreshVarName x ctx
  in  (x', addBinding (x', NameBinding) ctx)

mkFreshVarName :: String -> Context -> String
mkFreshVarName x [] = x
mkFreshVarName x ctx@(b:bs)
  | x == fst b = mkFreshVarName (x ++ "'") ctx
  | otherwise  = mkFreshVarName x bs
  
