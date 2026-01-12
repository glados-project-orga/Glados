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
import Control.Applicative
import StatementParser (parseType, parseBlock)

parseIsRef :: Parser Bool
parseIsRef = (symbol '&' *> pure True)
            <|> pure False

parseParam :: Parser Parameter
parseParam =
    (\isRef name typ -> Parameter name typ isRef)
        <$> parseIsRef
        <*> identifier
        <*> (keyword "->" *> parseType)

parseParams :: Parser [Parameter]
parseParams = symbol '(' *> sepBy parseParam comma <* symbol ')'

parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl =
    FunctionDecl
        <$> getSourcePos
        <*> (keyword "fun" *> identifier)
        <*>((parseParams)
            <|> (symbol '(' *> symbol ')' *> pure []))
        <*> (symbol ':' *> parseType)
        <*> parseBlock

parseFunction :: Parser Declaration
parseFunction =
    Function <$> parseFunctionDecl
