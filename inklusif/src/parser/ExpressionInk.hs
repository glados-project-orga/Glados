{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- ExpressionInk
-}

module ExpressionInk (
    parseExpression,
    parseLiteral
) where

import Ast
import Control.Applicative
import Parser

parseExpression :: Parser Expr
parseExpression = parseComparison

parseComparison :: Parser Expr
parseComparison =
    chainl1 parseAdditive comparisonOp

comparisonOp :: Parser (Expr -> Expr -> Expr)
comparisonOp =
        (BinOpExpr LessThan    <$ symbol '<')
    <|> (BinOpExpr GreaterThan <$ symbol '>')

parseAdditive :: Parser Expr
parseAdditive =
    chainl1 parseMultiplicative additiveOp

additiveOp :: Parser (Expr -> Expr -> Expr)
additiveOp =
        (BinOpExpr Add <$ symbol '+')
    <|> (BinOpExpr Sub <$ symbol '-')

parseMultiplicative :: Parser Expr
parseMultiplicative =
    chainl1 parseAtom multiplicativeOp

multiplicativeOp :: Parser (Expr -> Expr -> Expr)
multiplicativeOp =
    BinOpExpr Mul <$ symbol '*'

parseAtom :: Parser Expr
parseAtom =
        parseLiteralExpr
    <|> parseFunctionCall
    <|> (VarExpr <$> identifier)
    <|> parenthesized

parenthesized :: Parser Expr
parenthesized =
    symbol '(' *> parseExpression <* symbol ')'

parseLiteralExpr :: Parser Expr
parseLiteralExpr =
    LitExpr <$> parseLiteral

parseLiteral :: Parser Literal
parseLiteral =
        (IntLit <$> parseInt)
    <|> (BoolLit True  <$ keyword "true")
    <|> (BoolLit False <$ keyword "false")

parseFunctionCall :: Parser Expr
parseFunctionCall =
    CallExpression <$> callExpr

callExpr :: Parser CallExpr
callExpr =
    CallExpr
        <$> identifier
        <*> arguments

arguments :: Parser [Expr]
arguments =
    symbol '(' *> sepBy parseExpression comma <* symbol ')'

comma :: Parser ()
comma =
    symbol ',' *> pure ()
