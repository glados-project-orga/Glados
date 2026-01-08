{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- TypedefParser
-}

module TypedefParser (
    parseTypedef
    ) where

import Ast
import Parser
import StatementParser (parseType)

parseTypeDefDecl :: Parser TypedefDecl
parseTypeDefDecl =
    TypedefDecl <$> getSourcePos
                <*> (keyword "typedef" *> parseType)
                <*> identifier

parseTypedef :: Parser Declaration
parseTypedef = 
    Typedef <$> parseTypeDefDecl <* symbol ';'
