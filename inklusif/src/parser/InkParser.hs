{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- InkParser
-}

module InkParser (
    parseInkFile
    ) where

import Ast
import Parser
import FunctionParsing
import Control.Applicative
import EnumParsing
import ClassParser
import TypedefParser

parseDeclaration :: Parser Declaration
parseDeclaration =
        parseFunction
       <|> parseEnum
       <|> parseClass
       <|> parseTypedef

parseInkFile :: String -> IO (Either String [Declaration])
parseInkFile content =
    case runParser (parseMany parseDeclaration <* eof) (initialState content) of
        Right (decls, _) -> return $ Right decls
        Left err        -> return $ Left err
