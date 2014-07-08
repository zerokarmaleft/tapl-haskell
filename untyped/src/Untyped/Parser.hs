{-# LANGUAGE FlexibleContexts #-}

module Untyped.Parser where

import           Data.List
import           Text.Parsec
import qualified Text.Parsec.Token as P
import           Untyped.Context
import           Untyped.Syntax

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

lexer = P.makeTokenParser untypedDef

dot        = P.dot        lexer
identifier = P.identifier lexer
parens     = P.parens     lexer
reserved   = P.reserved   lexer

getVarIndex var ctx =
  case elemIndex var ctx of
    Just i  -> return i
    Nothing -> error "Unbound variable name"

parseVar =
  do var <- identifier
     ctx <- getState
     idx <- getVarIndex var ctx
     return $ TermVar idx (length ctx)

parseAbs =
  do reserved "lambda"
     var  <- identifier
     dot
     ctx  <- getState
     setState $ bindVarName var ctx
     term <- parseTerm
     setState ctx
     return $ TermAbs var term

parseNonApp = parseAbs <|> parseVar <|> parens parseTerm

parseTerm = 
  chainl1 (parseAbs <|> parseVar <|> parens parseTerm) 
          (return TermApp) 
