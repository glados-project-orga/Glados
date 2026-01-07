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

parseDeclaration :: Parser Declaration
parseDeclaration =
        parseFunction
--     <|> parseEnum
--     <|> parseStruct
--     <|> parseTypedef


parseInkFile :: String -> IO (Either String [Declaration])
parseInkFile content =
    case runParser (parseMany parseDeclaration) (initialState content) of
        Right (decl, _) -> return $ Right decl
        Left err -> return $ Left err