module Main where

import System.Environment
import Text.Parsec
import Untyped.Context
import Untyped.Evaluator
import Untyped.Parser

main :: IO ()
main =
  do args <- getArgs
     case args of
       [sourceFile] ->
         do parseTree <- fmap (runParser parseTerm mkContext "untypedi")
                             (readFile sourceFile)
            putStrLn $ show parseTree
            case parseTree of
              Right expr -> putStrLn $ "=> " ++ (show . eval) expr
              Left err   -> putStrLn $ show err
       _ -> putStrLn "Usage: untypedi <sourceFile>"
