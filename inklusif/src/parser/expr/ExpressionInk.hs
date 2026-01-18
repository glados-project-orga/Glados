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
    chainl1 parseAtom otherOp

parseUnary :: Parser Expr
parseUnary =
        (UnaryOpExpr PreInc <$ keyword "++" <*> parseAtom)
    <|> (UnaryOpExpr PreDec <$ keyword "--" <*> parseAtom)
    <|> (UnaryOpExpr Neg    <$ symbol '-' <*> parseAtom)
    <|> (UnaryOpExpr Not    <$ symbol '!' <*> parseAtom)
    <|> (UnaryOpExpr Ref    <$ symbol '&' <*> parseAtom)
    <|> (UnaryOpExpr Deref  <$ symbol '*' <*> parseAtom)

parseAtom :: Parser Expr
parseAtom =
        parseLiteralExpr
    <|> parseCastExpr
    <|> parseClassConstructor
    <|> parseFunctionCall
    <|> parseArrayLiteral
    <|> parseArrayVar
    <|> parseClassVar
    <|> parseUnary
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

parseClassAccess :: Parser ClassAccess
parseClassAccess = (ClassArrayAccess <$> identifier
        <*> (symbol '[' *> parseExpression <* symbol ']'))
    <|> (ClassMethodCall <$> callExpr)
    <|> (ClassClassAccess <$> identifier <*> (symbol '.' *> parseClassAccess))
    <|> (ClassVarAccess <$> identifier)

parseClassVar :: Parser Expr
parseClassVar = ClassVarExpr <$> identifier
    <*> (symbol '.' *> parseClassAccess)

parenthesized :: Parser Expr
parenthesized =
    symbol '(' *> parseExpression <* symbol ')'
