module Simplebool.Parser where

import           Data.Functor.Identity
import           Data.List
import           Simplebool.Context
import           Simplebool.Syntax
import           Text.Parsec
import qualified Text.Parsec.Token as P

type Parser a = ParsecT String Context Identity a

simpleboolDef :: P.LanguageDef st
simpleboolDef =
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
                                      , "Bool"
                                      ]
                , P.reservedNames   = [ "lambda"
                                      , "if"
                                      , "then"
                                      , "else"
                                      , "true"
                                      , "false"
                                      , "Bool"
                                      ]
                , P.caseSensitive   = True
                }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser simpleboolDef

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
parseTrue  = reserved "true"  >> return TermTrue

parseFalse :: Parser Term
parseFalse = reserved "false" >> return TermFalse

parseIf :: Parser Term
parseIf =
  do reservedOp "if"
     predicate  <- parseTerm
     reservedOp "then"
     consequent <- parseTerm
     reservedOp "else"
     antecedent <- parseTerm
     return $ TermIf predicate consequent antecedent

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
     return $ TermAbs var tyVar term

parseTypeBool :: Parser Type
parseTypeBool = reserved "Bool" >> return TypeBool

parseTypeArrow :: Parser Type
parseTypeArrow =
  do tyT1 <- parseTypeBool
     many space
     string "->"
     many space
     tyT2 <- parseType
     return $ TypeArrow tyT1 tyT2

parseType :: Parser Type
parseType =
  try parseTypeArrow <|> parseTypeBool <|> parens parseType

parseTypeAnnotation :: Parser Type
parseTypeAnnotation =
  do colon
     parseType

parseTerm :: Parser Term
parseTerm =
  chainl1 (parseTrue  <|>
           parseFalse <|>
           parseIf    <|>
           parseAbs   <|>
           parseVar   <|>
           parens parseTerm)
          (return TermApp)
