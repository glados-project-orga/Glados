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
parseLiteral =
        parseNumberLiteral
    <|> (FloatLit  <$> parseFloat)
    <|> (DoubleLit <$> parseDouble)
    <|> (CharLit <$> (parseChar '\''
        *> parseAnyChar (filter (/= '\'') (map toEnum [32..126])) <* parseChar '\''))
    <|> (BoolLit True  <$ keyword "true")
    <|> (BoolLit False <$ keyword "false")
    <|> (StringLit <$> parseStringLiteral)

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
        | n <= fromIntegral (maxBound :: Int32)
            = IntLit  (fromIntegral n)
        | otherwise
            = LongLit (fromIntegral n)
