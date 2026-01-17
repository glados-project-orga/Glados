{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- Literal
-}

module Literal (
    parseLiteral,
    parseLiteralExpr
    ) where

import Ast
import Parser
import Control.Applicative
import Data.Int (Int32)

parseLiteralExpr :: Parser Expr
parseLiteralExpr =
    LitExpr <$> parseLiteral

parseLiteral :: Parser Literal
parseLiteral = (DoubleLit <$> parseDouble)
    <|> (FloatLit  <$> parseFloat)
    <|> parseNumberLiteral
    <|> parseCharLiteral
    <|> (BoolLit True  <$ keyword "true")
    <|> (BoolLit False <$ keyword "false")
    <|> (StringLit <$> parseStringLiteral)

parseCharLiteral :: Parser Literal
parseCharLiteral =
    CharLit <$> (
        (parseChar '\''
        *> parseAnyChar (filter (`notElem` ['\'', '\\']) (map toEnum [32..126])) <* parseChar '\'')
        <|> parseChar '\'' *> parseSpecialChar <* parseChar '\'')

parseSpecialChar :: Parser Char
parseSpecialChar =
    parseChar '\\' *>
    (   ('\n'  <$ parseChar 'n')
    <|> ('\t'  <$ parseChar 't')
    <|> ('\r'  <$ parseChar 'r')
    )

parseStringLiteral :: Parser String
parseStringLiteral = 
    parseChar '"' *> parseMany stringChar <* parseChar '"'
  where
    stringChar = parseAnyChar (filter (/= '"') (map toEnum [32..126]))

parseNumberLiteral :: Parser Literal
parseNumberLiteral =
    decide <$> parseNumber
  where
    decide n
        | n >= fromIntegral (minBound :: Int32)
       && n <= fromIntegral (maxBound :: Int32)
            = IntLit  (fromIntegral n)
        | otherwise
            = LongLit (fromIntegral n)
