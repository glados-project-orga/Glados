{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- ExpressionInk
-}

module ExpressionInk (
    parseExpression
    ) where

import Ast
import Control.Applicative
import Parser
import OpExpression
import Literal

parseExpression :: Parser Expr
parseExpression = parseComparison

parseComparison :: Parser Expr
parseComparison =
    chainl1 parseAdditive comparisonOp

parseAdditive :: Parser Expr
parseAdditive =
    chainl1 parseMultiplicative additiveOp

parseMultiplicative :: Parser Expr
parseMultiplicative =
    chainl1 parseEqual multiplicativeOp

parseEqual :: Parser Expr
parseEqual =
    chainl1 parseOther equalOp

parseOther :: Parser Expr
parseOther =
    chainl1 parseUnary otherOp

parseUnary :: Parser Expr
parseUnary =
        (UnaryOpExpr Neg    <$ symbol '-' <*> parseUnary)
    <|> (UnaryOpExpr Not    <$ symbol '!' <*> parseUnary)
    <|> (UnaryOpExpr PreInc <$ keyword "++" <*> parseUnary)
    <|> (UnaryOpExpr PreDec <$ keyword "--" <*> parseUnary)
    <|> (UnaryOpExpr Ref    <$ symbol '&' <*> parseUnary)
    <|> (UnaryOpExpr Deref  <$ symbol '*' <*> parseUnary)
    <|> parseAtom

parseAtom :: Parser Expr
parseAtom =
        parseLiteralExpr
    <|> parseCastExpr
    <|> parseClassConstructor
    <|> parseFunctionCall
    <|> parseArrayLiteral
    <|> parseArrayVar
    <|> parseClassVar
    <|> (VarExpr <$> identifier)
    <|> parenthesized

parseCast :: Parser Type
parseCast = (keyword "int" *> pure IntType)
    <|> (keyword "void" *> pure VoidType)
    <|> (keyword "long" *> pure LongType)
    <|> (keyword "bool" *> pure BoolType)
    <|> (keyword "string" *> pure StringType)
    <|> (keyword "float" *> pure FloatType)
    <|> (keyword "double" *> pure DoubleType)
    <|> (keyword "char" *> pure CharType)
    <|> (pure CustomType) <*> identifier

parseCastExpr :: Parser Expr
parseCastExpr =
    CastExpr <$> (symbol '(' *> parseCast <* symbol ')')
             <*> parseAtom

parseClassConstructor :: Parser Expr
parseClassConstructor =
    ClassConstructorExpr <$> (keyword "new" *> identifier)
        <*> arguments

parseArrayLiteral :: Parser Expr
parseArrayLiteral =
    ArrayLiteral <$> (symbol '[' *> sepBy parseExpression comma <* symbol ']')

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

parseArrayVar :: Parser Expr
parseArrayVar = ArrayVarExpr <$> identifier
    <*> (symbol '[' *> parseExpression <* symbol ']')

parseClassVar :: Parser Expr
parseClassVar = ClassVarExpr <$> identifier
    <*> (symbol '.' *> parseExpression)

parenthesized :: Parser Expr
parenthesized =
    symbol '(' *> parseExpression <* symbol ')'


