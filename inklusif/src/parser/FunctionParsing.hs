{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- FunctionParsing
-}

module FunctionParsing (
    parseFunction
    ) where

import Ast
import Parser
import StatementParser (parseType, parseBlock)

parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl =
    FunctionDecl
        <$> getSourcePos
        <*> (keyword "fun" *> identifier)
        <*> (symbol '(' *> symbol ')' *> pure [])
        <*> (symbol ':' *> parseType)
        <*> parseBlock

parseFunction :: Parser Declaration
parseFunction =
    Function <$> parseFunctionDecl
