module Arith.Parser (parseTerm) where

import Arith.Syntax
import Text.Parsec
import Text.Parsec.String

parseTrue :: Parser Term
parseTrue = string "true" >> return TermTrue

parseFalse :: Parser Term
parseFalse = string "false" >> return TermFalse

parseIf :: Parser Term
parseIf =
  do string "if"
     space
     predicate <- parseTerm
     space
     string "then"
     space
     consequent <- parseTerm
     space
     string "else"
     space
     antecedent <- parseTerm
     return $ TermIf predicate consequent antecedent

parseZero :: Parser Term
parseZero = string "0" >> return TermZero

parseSucc :: Parser Term
parseSucc =
  do string "succ"
     space
     spaces
     t <- parseTerm
     return $ TermSucc t
     
parsePred :: Parser Term
parsePred =
  do string "pred"
     space
     spaces
     t <- parseTerm
     return $ TermPred t
     
parseIsZero :: Parser Term
parseIsZero =
  do string "zero?"
     space
     t <- parseTerm
     return $ TermIsZero t
     
parseTerm :: Parser Term
parseTerm =
  parseTrue   <|>
  parseFalse  <|>
  parseIf     <|>
  parseZero   <|>
  parseSucc   <|>
  parsePred   <|>
  parseIsZero <|>
  between (string "(") (string ")") parseTerm
