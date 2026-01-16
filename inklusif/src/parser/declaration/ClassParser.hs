{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- ClassParser
-}

module ClassParser (
    parseClass
    ) where

import Ast
import Parser
import Control.Applicative
import FunctionParsing (parseParams)
import StatementParser (parseType, parseBlock)

parseClassMethod :: Parser FunctionDecl
parseClassMethod =
    FunctionDecl
        <$> getSourcePos
        <*> (keyword "method" *> identifier)
        <*> ((parseParams)
            <|> (symbol '(' *> symbol ')' *> pure []))
        <*> (symbol ':' *> parseType)
        <*> parseBlock

parseClassMethods :: Parser [FunctionDecl]
parseClassMethods = (many parseClassMethod)

parseClassField :: Parser StructField
parseClassField =
    StructField
        <$> (identifier <* keyword "->")
        <*> (parseType <* symbol ';')

parseClassFields :: Parser [StructField]
parseClassFields = (many parseClassField)

parseClassDecl :: Parser ClassDecl
parseClassDecl =
    ClassDecl
        <$> getSourcePos
        <*> (keyword "class" *> identifier)
        <*> (symbol '{' *> parseClassFields)
        <*> parseClassMethods <* symbol '}'

parseClass :: Parser Declaration
parseClass = Class <$> parseClassDecl

