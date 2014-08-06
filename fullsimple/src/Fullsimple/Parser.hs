module Fullsimple.Parser where

import Debug.Trace

import           Data.Functor.Identity
import           Data.List
import           Fullsimple.Context
import           Fullsimple.Terms
import           Fullsimple.Types
import           Text.Parsec
import qualified Text.Parsec.Token as P

type Parser a = ParsecT String Context Identity a

fullsimpleDef :: P.LanguageDef st
fullsimpleDef =
  P.LanguageDef { P.commentStart    = ""
                , P.commentEnd      = ""
                , P.commentLine     = ""
                , P.nestedComments  = True
                , P.identStart      = letter
                , P.identLetter     = alphaNum
                , P.opStart         = letter
                , P.opLetter        = alphaNum
                , P.reservedOpNames = [ "lambda"
                                      , "if"
                                      , "then"
                                      , "else"
                                      , "true"
                                      , "false"
                                      , "succ"
                                      , "pred"
                                      , "zero?"
                                      , "Bool"
                                      , "Nat"
                                      ]
                , P.reservedNames   = [ "lambda"
                                      , "if"
                                      , "then"
                                      , "else"
                                      , "true"
                                      , "false"
                                      , "succ"
                                      , "pred"
                                      , "zero?"
                                      , "Bool"
                                      , "Nat"
                                      ]
                , P.caseSensitive   = True
                }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser fullsimpleDef

colon :: ParsecT String u Identity String
colon = P.colon lexer

dot :: ParsecT String u Identity String
dot        = P.dot        lexer

identifier :: ParsecT String u Identity String
identifier = P.identifier lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens     = P.parens     lexer

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp lexer

reserved :: String -> ParsecT String u Identity ()
reserved   = P.reserved   lexer

parseTrue :: Parser Term
parseTrue  = traceM "Parsing <true>" >> reserved "true"  >> return TermTrue

parseFalse :: Parser Term
parseFalse = traceM "Parsing <false>" >> reserved "false" >> return TermFalse

parseIf :: Parser Term
parseIf =
  do reservedOp "if"
     predicate  <- parseTerm
     reservedOp "then" 
     consequent <- parseTerm 
     reservedOp "else" 
     antecedent <- parseTerm
     traceM "Parsing <if>"
     return $ TermIf predicate consequent antecedent
     
parseZero :: Parser Term
parseZero = traceM "Parsing <0>" >> string "0" >> return TermZero

parseSucc :: Parser Term
parseSucc =
  do reservedOp "succ"
     n <- parseTerm
     traceM "Parsing <succ>"
     return $ TermSucc n
     
parsePred :: Parser Term
parsePred =
  do reservedOp "pred" 
     n <- parseTerm
     traceM "Parsing <pred>"
     return $ TermPred n
     
parseIsZero :: Parser Term
parseIsZero =
  do reservedOp "zero?"
     n <- parseTerm
     traceM "Parsing <is-zero>"
     return $ TermIsZero n

getVarIndex :: (Monad m, Eq a) => a -> [(a,b)] -> m Int
getVarIndex var ctx =
  case findIndex ((== var) . fst) ctx of
    Just i  -> return i
    Nothing -> error "Unbound variable name"

parseVar :: Parser Term
parseVar =
  do var <- identifier
     ctx <- getState
     idx <- getVarIndex var ctx
     traceM "Parsing <var>"
     return $ TermVar idx (length ctx)

parseAbs :: Parser Term
parseAbs =
  do reservedOp "lambda" 
     var   <- identifier 
     tyVar <- parseTypeAnnotation
     dot
     ctx   <- getState
     setState $ addBinding (var, VarBinding tyVar) ctx 
     term  <- parseTerm
     setState ctx
     traceM "Parsing <lambda>"
     return $ TermAbs var tyVar term

parseTypeBool :: Parser Type
parseTypeBool = reserved "Bool" >> traceM "Parsing <type-bool>" >> return TypeBool

parseTypeNat :: Parser Type
parseTypeNat = reserved "Nat" >> traceM "Parsing <type-nat>" >> return TypeNat

parseTypeArrow :: Parser Type
parseTypeArrow =
  do tyT1 <- parseTypeBool <|> parseTypeNat
     many space
     string "->"
     many space
     tyT2 <- parseType
     traceM "Parsing <type-arrow>"
     return $ TypeArrow tyT1 tyT2

parseType :: Parser Type
parseType =
  try parseTypeArrow <|> parseTypeBool <|> parseTypeNat <|> parens parseType

parseTypeAnnotation :: Parser Type
parseTypeAnnotation =
  do colon
     parseType

parseTerm :: Parser Term
parseTerm =
  chainl1 (parseTrue   <|>
           parseFalse  <|>
           parseIf     <|>
           parseZero   <|>
           parseSucc   <|>
           parsePred   <|>
           parseIsZero <|>
           parseAbs    <|>
           parseVar    <|>
           parens parseTerm)
          (traceM "Parsing <lambda-app>" >> return TermApp)
