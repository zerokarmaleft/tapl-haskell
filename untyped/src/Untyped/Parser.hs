{-# LANGUAGE FlexibleContexts #-}

module Untyped.Parser where

import           Data.Functor.Identity
import           Data.List
import           Text.Parsec
import qualified Text.Parsec.Token as P
import           Untyped.Context
import           Untyped.Syntax

type Parser a = ParsecT String Context Identity a

untypedDef :: P.LanguageDef st
untypedDef =
  P.LanguageDef { P.commentStart    = ""
                , P.commentEnd      = ""
                , P.commentLine     = ""
                , P.nestedComments  = True
                , P.identStart      = letter
                , P.identLetter     = alphaNum
                , P.opStart         = letter
                , P.opLetter        = alphaNum
                , P.reservedOpNames = [ "lambda" ]
                , P.reservedNames   = []
                , P.caseSensitive  = True
                }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser untypedDef

dot :: ParsecT String u Identity String
dot        = P.dot        lexer

identifier :: ParsecT String u Identity String
identifier = P.identifier lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens     = P.parens     lexer

reserved :: String -> ParsecT String u Identity ()
reserved   = P.reserved   lexer

getVarIndex :: (Monad m, Eq a) => a -> [a] -> m Int
getVarIndex var ctx =
  case elemIndex var ctx of
    Just i  -> return i
    Nothing -> error "Unbound variable name"

parseVar :: Parser Term
parseVar =
  do var <- identifier
     ctx <- getState
     idx <- getVarIndex var ctx
     return $ TermVar idx (length ctx)

parseAbs :: Parser Term
parseAbs =
  do reserved "lambda"
     var  <- identifier
     dot
     ctx  <- getState
     setState $ bindVarName var ctx
     term <- parseTerm
     setState ctx
     return $ TermAbs var term

parseTerm :: Parser Term
parseTerm = 
  chainl1 (parseAbs <|> parseVar <|> parens parseTerm) 
          (return TermApp) 
