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
additiveOp = (BinOpExpr AddEqual <$ keyword "+=")
    <|> (BinOpExpr SubEqual <$ keyword "-=")
    <|> (BinOpExpr Add <$ symbol '+')
    <|> (BinOpExpr Sub <$ symbol '-')

parseMultiplicative :: Parser Expr
parseMultiplicative =
    chainl1 parseEqual multiplicativeOp

multiplicativeOp :: Parser (Expr -> Expr -> Expr)
multiplicativeOp = (BinOpExpr MulEqual <$ keyword "*=")
    <|> (BinOpExpr DivEqual <$ keyword "/=")
    <|> (BinOpExpr ModEqual <$ keyword "%=")
    <|> (BinOpExpr Mul <$ symbol '*')
    <|> (BinOpExpr Div <$ symbol '/')
    <|> (BinOpExpr Mod <$ symbol '%')

parseEqual :: Parser Expr
parseEqual =
    chainl1 parseOther equalOp

equalOp :: Parser (Expr -> Expr -> Expr)
equalOp =
        (BinOpExpr Equal <$ keyword "==")
    <|> (BinOpExpr NotEqual <$ keyword "!=")
    <|> (BinOpExpr LessEqual <$ keyword "<=")
    <|> (BinOpExpr GreaterEqual <$ keyword ">=")

parseOther :: Parser Expr
parseOther =
    chainl1 parseAtom otherOp

otherOp :: Parser (Expr -> Expr -> Expr)
otherOp = 
        (BinOpExpr And <$ keyword "&&")
    <|> (BinOpExpr Or <$ keyword "||")

parseClassVar :: Parser Expr
parseClassVar = ClassVarExpr <$> identifier
    <*> (symbol '.' *> parseExpression)

parseClassConstructor :: Parser Expr
parseClassConstructor =
    ClassConstructorExpr <$> (keyword "new" *> identifier)
        <*> arguments

parseAtom :: Parser Expr
parseAtom =
        parseLiteralExpr
    <|> parseClassConstructor
    <|> parseFunctionCall
    <|> parseStructLiteral
    <|> parseArrayLiteral
    <|> parseArrayVar
    <|> parseClassVar
    <|> (VarExpr <$> identifier)
    <|> parenthesized

parseArrayVar :: Parser Expr
parseArrayVar = ArrayVarExpr <$> identifier
    <*> (symbol '[' *> parseExpression <* symbol ']')

-- traceInput :: String -> Parser a -> Parser a
-- traceInput label (Parser p) =
--     Parser $ \st ->
--         trace
--             (label ++ " | next input = " ++ take 40 (input st)
--              )
--             (p st)

parseArrayLiteral :: Parser Expr
parseArrayLiteral =
    ArrayLiteral <$> (symbol '[' *> sepBy parseExpression comma <* symbol ']')

parseStringLiteral :: Parser String
parseStringLiteral = 
    parseChar '"' *> parseMany stringChar <* parseChar '"'
  where
    stringChar = parseAnyChar (filter (/= '"') (map toEnum [32..126]))

parseStructLiteral :: Parser Expr
parseStructLiteral =
    StructLiteral <$> parseStructFields

parseStructFields :: Parser [(String, Expr)]
parseStructFields =
    symbol '{' *> sepBy structField comma <* symbol '}'
  where
    structField = (,) <$> identifier <*> (symbol ':' *> parseExpression)

parenthesized :: Parser Expr
parenthesized =
    symbol '(' *> parseExpression <* symbol ')'

parseLiteralExpr :: Parser Expr
parseLiteralExpr =
    LitExpr <$> parseLiteral

parseLiteral :: Parser Literal
parseLiteral = (FloatLit <$> parseFloat)
    <|> (DoubleLit <$> parseDouble)
    <|> (IntLit <$> parseInt)
    <|> (BoolLit True  <$ keyword "true")
    <|> (StringLit <$> parseStringLiteral)
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
