module Simplebool.Context where

import Control.Monad
import Data.List

data Type = TypeBool
          | TypeArrow Type Type
          deriving (Eq, Show)

type Context = [(String, Binding)]

data Binding = NameBinding
             | VarBinding Type
             deriving (Show)

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

freshVarName = undefined
