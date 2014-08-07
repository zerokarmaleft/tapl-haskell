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
                    , Token.opStart         = oneOf "-"
                    , Token.opLetter        = oneOf ">"
                    , Token.reservedOpNames = [ "->" ]
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

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep   = Token.commaSep   lexer

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

parseProjections :: Parser [Term]
parseProjections = commaSep parseTerm

parseProduct :: Parser Term
parseProduct =
  do ts <- braces parseProjections
     traceM "Parsing <product-proj>"
     return $ TermProduct ts

parseProj :: Parser (Term -> Term)
parseProj =
  do dot
     j <- many1 digit
     traceM "Parsing <proj>"
     return $ TermProj ((read j) - 1)

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

parseTypeArrow :: Parser (Type -> Type -> Type)
parseTypeArrow = reservedOp "->" >> traceM "Parsing <type-arrow>" >> return TypeArrow

parseType :: Parser Type
parseType = parseTypeExpr

parseNonArrowType :: Parser Type
parseNonArrowType = parseTypeBool <|> parseTypeNat <|> parens parseType

parseTypeAnnotation :: Parser Type
parseTypeAnnotation = colon >> parseType

typeOps = [ [Expr.Infix parseTypeArrow Expr.AssocLeft] ]

parseTypeExpr :: Parser Type
parseTypeExpr = Expr.buildExpressionParser typeOps parseNonArrowType

parseTerm :: Parser Term
parseTerm = chainl1 parseTermExpr (traceM "Parsing <lambda-app>" >> return TermApp)

parseNonAppTerm :: Parser Term
parseNonAppTerm = (parseTrue    <|>
                   parseFalse   <|>
                   parseIf      <|>
                   parseZero    <|>
                   parseSucc    <|>
                   parsePred    <|>
                   parseIsZero  <|>
                   parseProduct <|>
                   parseAbs     <|>
                   parseVar     <|>
                   parens parseTerm)

termOps = [ [Expr.Postfix parseProj] ]

parseTermExpr :: Parser Term
parseTermExpr = Expr.buildExpressionParser termOps parseNonAppTerm
