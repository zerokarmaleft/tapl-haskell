module Main where

import Simplebool.Context
import Simplebool.Evaluator
import Simplebool.Parser
import Simplebool.Syntax
import Simplebool.Typechecker
import System.Environment
import Text.Parsec

main :: IO ()
main =
  do args <- getArgs
     case args of
       [sourceFile] ->
         do let ctx = mkContext
            parseTree <- fmap (parse parseTerm "simplebool") $ readFile sourceFile
            case parseTree of
              Right expr -> 
                case typeOf ctx expr of
                  Right ty -> putStrLn $ "=> " ++ (showTerm ctx . eval) expr
                  Left err -> putStrLn $ "Type Error: " ++ show err 
              Left err -> putStrLn $ "Parsing Error: " ++ show err 
       _ -> putStrLn "Usage: simplebool <sourceFile>"
       
