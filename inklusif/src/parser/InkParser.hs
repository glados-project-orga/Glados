{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- InkParser
-}

module InkParser (
    parseInkFile
    ) where

import Ast (Declaration)

parseInkFile :: String -> IO (Either String Declaration)
parseInkFile _ = return $ Left "Parsing not yet implemented."