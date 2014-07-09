module Main where

import System.Environment
import Text.Parsec
import Untyped.Context
import Untyped.Evaluator
import Untyped.Parser
import Untyped.Syntax

main :: IO ()
main =
  do args <- getArgs
     case args of
       [sourceFile] ->
         do let ctx = mkContext
            parseTree <- fmap (runParser parseTerm ctx "untyped") (readFile sourceFile)
            case parseTree of
              Right expr -> putStrLn $ (show parseTree) ++ "\n=> " ++ (show . eval) expr
              Left err   -> putStrLn $ show err
       _ -> putStrLn "Usage: untyped <sourceFile>"
