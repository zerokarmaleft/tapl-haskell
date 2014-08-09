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
                    , Token.opStart         = oneOf "-a"
                    , Token.opLetter        = oneOf ">s"
                    , Token.reservedOpNames = [ "->"
                                              , "as"
                                              ]
                    , Token.reservedNames   = [ "unit"
                                              , "_"
                                              , "true"
                                              , "false"
                                              , "if"
                                              , "then"
                                              , "else"
                                              , "0"
                                              , "succ"
                                              , "pred"
                                              , "zero?"
                                              , "lambda"
                                              , "Bool"
                                              , "Nat"
                                              ]
                    , Token.caseSensitive   = True
                    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser fullsimpleDef

comma :: ParsecT String u Identity String
comma      = Token.comma      lexer

semi :: ParsecT String u Identity String
semi       = Token.semi       lexer

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

-- Term Expressions
--

parseUnit :: Parser Term
parseUnit = reserved "unit" >> traceM "Parsing <unit>" >> return TermUnit

parseAscription :: Parser (Term  -> Term)
parseAscription =
  do reserved "as"
     tyT <- parseType
     traceM "Parsing <ascription>"
     return $ (flip TermAscription) tyT

parseTrue :: Parser Term
parseTrue  = reserved "true" >> traceM "Parsing <true>" >> return TermTrue

parseFalse :: Parser Term
parseFalse = reserved "false" >> traceM "Parsing <false>" >> return TermFalse

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
parseZero = reserved "0" >> traceM "Parsing <0>" >> return TermZero

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
     i <- many1 digit
     traceM "Parsing <proj>"
     return $ TermProj ((read i) - 1)

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

parseWildcardAbs :: Parser Term
parseWildcardAbs =
  do reservedOp "lambda"
     reserved "_"
     tyVar <- parseTypeAnnotation
     dot
     ctx   <- getState
     term  <- parseTerm
     traceM "Parsing <wildcard-lambda>"
     return $ TermAbs "_" tyVar term

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

parseSequence :: Parser (Term -> Term -> Term)
parseSequence =
  do semi
     ctx <- getState
     traceM "Parsing <sequence>"
     return $ \t1 t2 -> TermApp (TermAbs "_" TypeUnit t2) t1

parseTerm :: Parser Term
parseTerm = chainl1 parseTermExpr (traceM "Parsing <lambda-app>" >> return TermApp)

parseNonAppTerm :: Parser Term
parseNonAppTerm = (parseUnit        <|>
                   parseTrue        <|>
                   parseFalse       <|>
                   parseIf          <|>
                   parseZero        <|>
                   parseSucc        <|>
                   parsePred        <|>
                   parseIsZero      <|>
                   parseProduct     <|>
                   (try parseWildcardAbs <|> parseAbs) <|>
                   parseVar         <|>
                   parens parseTerm)

termOps = [ [Expr.Postfix parseAscription               ]
          , [Expr.Postfix parseProj                     ]
          , [Expr.Infix   parseSequence   Expr.AssocLeft] ]

parseTermExpr :: Parser Term
parseTermExpr = Expr.buildExpressionParser termOps parseNonAppTerm

-- Type Expressions
--

parseTypeUnit :: Parser Type
parseTypeUnit = reserved "Unit" >> traceM "Parsing <type-unit>" >> return TypeUnit

parseTypeBool :: Parser Type
parseTypeBool = reserved "Bool" >> traceM "Parsing <type-bool>" >> return TypeBool

parseTypeNat :: Parser Type
parseTypeNat = reserved "Nat" >> traceM "Parsing <type-nat>" >> return TypeNat

parseTypeProduct :: Parser Type
parseTypeProduct =
  do ts <- braces (commaSep parseTypeExpr)
     traceM "Parsing <type-product>"
     return $ TypeProduct ts

parseTypeArrow :: Parser (Type -> Type -> Type)
parseTypeArrow = reservedOp "->" >> traceM "Parsing <type-arrow>" >> return TypeArrow

parseType :: Parser Type
parseType = parseTypeExpr

parseNonArrowType :: Parser Type
parseNonArrowType =
  parseTypeUnit    <|>
  parseTypeBool    <|>
  parseTypeNat     <|>
  parseTypeProduct <|>
  parens parseType

parseTypeAnnotation :: Parser Type
parseTypeAnnotation = colon >> parseType

typeOps = [ [Expr.Infix parseTypeArrow Expr.AssocLeft] ]

parseTypeExpr :: Parser Type
parseTypeExpr = Expr.buildExpressionParser typeOps parseNonArrowType

