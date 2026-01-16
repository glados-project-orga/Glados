{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- EnumParsing
-}

module EnumParsing (
    parseEnum
    ) where

import Ast
import Parser
import Control.Applicative

parseEnumFieldItem :: Parser EnumField
parseEnumFieldItem =
    EnumField
        <$> identifier
        <*> ((symbol '=' *> (Just <$> parseInt)) <|> pure Nothing)

parseEnumField :: Parser [EnumField]
parseEnumField = (sepBy parseEnumFieldItem (symbol ','))

parseEnumDecl :: Parser EnumDecl
parseEnumDecl =
    EnumDecl
        <$> getSourcePos
        <*> (keyword "enum" *> identifier)
        <*> (symbol '{' *> parseEnumField <* symbol '}')

parseEnum :: Parser Declaration
parseEnum = Enum <$> parseEnumDecl