{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- OpExpression
-}

module OpExpression (
    comparisonOp,
    additiveOp,
    multiplicativeOp,
    equalOp,
    otherOp
    ) where

import Ast
import Parser
import Control.Applicative ((<|>))

comparisonOp :: Parser (Expr -> Expr -> Expr)
comparisonOp =
        (BinOpExpr LessThan    <$ symbol '<')
    <|> (BinOpExpr GreaterThan <$ symbol '>')


additiveOp :: Parser (Expr -> Expr -> Expr)
additiveOp = (BinOpExpr AddEqual <$ keyword "+=")
    <|> (BinOpExpr SubEqual <$ keyword "-=")
    <|> (BinOpExpr Add <$ symbol '+')
    <|> (BinOpExpr Sub <$ symbol '-')

multiplicativeOp :: Parser (Expr -> Expr -> Expr)
multiplicativeOp = (BinOpExpr MulEqual <$ keyword "*=")
    <|> (BinOpExpr DivEqual <$ keyword "/=")
    <|> (BinOpExpr ModEqual <$ keyword "%=")
    <|> (BinOpExpr Mul <$ symbol '*')
    <|> (BinOpExpr Div <$ symbol '/')
    <|> (BinOpExpr Mod <$ symbol '%')

equalOp :: Parser (Expr -> Expr -> Expr)
equalOp =
        (BinOpExpr Equal <$ keyword "==")
    <|> (BinOpExpr NotEqual <$ keyword "!=")
    <|> (BinOpExpr LessEqual <$ keyword "<=")
    <|> (BinOpExpr GreaterEqual <$ keyword ">=")

otherOp :: Parser (Expr -> Expr -> Expr)
otherOp = 
        (BinOpExpr And <$ keyword "&&")
    <|> (BinOpExpr Or <$ keyword "||")
