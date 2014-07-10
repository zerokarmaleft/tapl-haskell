module Main where

import Arith.Evaluator
import Arith.Parser
import System.Environment
import Text.Parsec
import Tyarith.Typechecker

main :: IO ()
main =
  do args <- getArgs
     case args of
       [sourceFile] -> 
         do parseTree <- fmap (parse parseTerm "arith") $ readFile sourceFile
            case parseTree of
              Right expr -> 
                case typeOf expr of
                  Right ty -> putStrLn $ "=> " ++ (show . eval) expr
                  Left err -> putStrLn $ "Type Error: " ++ (show err) 
              Left err -> putStrLn $ "Parsing Error: " ++ (show err)
       _ -> putStrLn "Usage: tyarith <sourceFile>"
