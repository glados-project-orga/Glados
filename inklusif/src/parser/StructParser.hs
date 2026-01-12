{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- StructParser
-}

module StructParser (
    parseStruct
    ) where

import Ast
import Parser
import Control.Applicative
import StatementParser (parseType)

parseStructField :: Parser StructField
parseStructField =
    StructField
        <$> (identifier <* keyword "->")
        <*> (parseType <* symbol ';')

parseStructFields :: Parser [StructField]
parseStructFields = (many parseStructField)

parseStructDecl :: Parser StructDecl
parseStructDecl =
    StructDecl
        <$> getSourcePos
        <*> (keyword "struct" *> identifier)
        <*> (symbol '{' *> parseStructFields <* symbol '}' <* symbol ';')

parseStruct :: Parser Declaration
parseStruct = Struct <$> parseStructDecl

