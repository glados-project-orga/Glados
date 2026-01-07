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
import Debug.Trace (trace)
import StatementParser (parseType)

traceInput :: String -> Parser a -> Parser a
traceInput label (Parser p) =
    Parser $ \st ->
        trace
            (label ++ " | next input = " ++ take 40 (input st)
             )
            (p st)

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

