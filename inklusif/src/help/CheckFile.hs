{-
-- EPITECH PROJECT, 2025
-- glados-repo
-- File description:
-- CheckFile
-}

module CheckFile (
    checkFile
    ) where

import System.Directory (doesFileExist, getFileSize)

checkFile :: FilePath -> IO (Either String String)
checkFile path = doesFileExist path >>= \exists ->
    if not exists
       then return $ Left $ "File not found: " ++ path
       else (Right <$> readFile path) >>= \content ->
            getFileSize path >>= \size ->
            return $ if size <= 0
                then Left "File is not completed, check documentation on how to code in Inklusif."
                else content