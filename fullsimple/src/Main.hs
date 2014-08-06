module Main where

import Fullsimple.Context
import Fullsimple.Evaluator
import Fullsimple.Parser
import Fullsimple.Printer
import Fullsimple.Typechecker
import System.Environment
import Text.Parsec

main :: IO ()
main =
  do args <- getArgs
     case args of
       [sourceFile] -> 
         do let ctx = mkContext
            parseTree <- fmap (runParser parseTerm ctx "fullsimple")
                             (readFile sourceFile)
            case parseTree of
              Right expr ->
                case typeOf ctx expr of
                  Right ty ->
                    do putStrLn $ "Evaluating (" ++ (show expr) ++ ")"
                       putStrLn $ "=> " ++ (printTerm ctx . eval) expr
                  Left err ->
                    putStrLn $ "Type Error in (" ++ show expr ++ "): " ++ show err
              Left err -> putStrLn $ "Parsing Error: " ++ show err 
       _ -> putStrLn "Usage: fullsimple <sourceFile>"

