module Fullsimple.Parser where

import Debug.Trace

import           Data.Functor.Identity
import           Data.List
import           Fullsimple.Context
import           Fullsimple.Terms
import           Fullsimple.Types
import           Text.Parsec
import qualified Text.Parsec.Expr  as Expr
import qualified Text.Parsec.Token as Token

type Parser a = ParsecT String Context Identity a

fullsimpleDef :: Token.LanguageDef st
fullsimpleDef =
  Token.LanguageDef { Token.commentStart    = ""
                    , Token.commentEnd      = ""
                    , Token.commentLine     = ""
                    , Token.nestedComments  = True
                    , Token.identStart      = letter
                    , Token.identLetter     = alphaNum
                    , Token.opStart         = letter
                    , Token.opLetter        = alphaNum
                    , Token.reservedOpNames = [ "lambda"
                                          , "if"
                                          , "then"
                                          , "else"
                                          , "true"
                                          , "false"
                                          , "0"
                                          , "succ"
                                          , "pred"
                                          , "zero?"
                                          , ".1"
                                          , ".2"
                                          , "Bool"
                                          , "Nat"
                                          ]
                    , Token.reservedNames   = [ "lambda"
                                          , "if"
                                          , "then"
                                          , "else"
                                          , "true"
                                          , "false"
                                          , "0"
                                          , "succ"
                                          , "pred"
                                          , "zero?"
                                          , ".1"
                                          , ".2"
                                          , "Bool"
                                          , "Nat"
                                          ]
                    , Token.caseSensitive   = True
                    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser fullsimpleDef

comma :: ParsecT String u Identity String
comma      = Token.comma      lexer

colon :: ParsecT String u Identity String
colon      = Token.colon      lexer

dot :: ParsecT String u Identity String
dot        = Token.dot        lexer

identifier :: ParsecT String u Identity String
identifier = Token.identifier lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens     = Token.parens     lexer

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces     = Token.braces     lexer

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = Token.reservedOp lexer

reserved :: String -> ParsecT String u Identity ()
reserved   = Token.reserved   lexer

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
parseZero = traceM "Parsing <0>" >> reserved "0" >> return TermZero

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

parseProjections :: Parser (Term, Term)
parseProjections =
  do t1 <- parseTerm
     comma
     t2 <- parseTerm
     return (t1, t2)

parsePair :: Parser Term
parsePair =
  do (t1, t2) <- braces parseProjections
     return $ TermPair t1 t2

parseProj1 :: Parser (Term -> Term)
parseProj1 = reservedOp ".1" >> return TermProj1

parseProj2 :: Parser (Term -> Term)
parseProj2 = reservedOp ".2" >> return TermProj2

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
parseTerm = chainl1 parseTermExpr (traceM "Parsing <lambda-app>" >> return TermApp)

parseNonAppTerm :: Parser Term
parseNonAppTerm = (parseTrue    <|>
                   parseFalse  <|>
                   parseIf     <|>
                   parseZero   <|>
                   parseSucc   <|>
                   parsePred   <|>
                   parseIsZero <|>
                   parsePair   <|>
                   parseAbs    <|>
                   parseVar    <|>
                   parens parseTerm)

termOps = [ [Expr.Postfix parseProj1]
          , [Expr.Postfix parseProj2]
          ]

parseTermExpr :: Parser Term
parseTermExpr = Expr.buildExpressionParser termOps parseNonAppTerm
