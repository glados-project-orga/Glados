{-
-- EPITECH PROJECT, 2025
-- glados-repo
-- File description:
-- Args
-}

module Args (
    checkArgs,
    ) where

import System.FilePath (takeExtension)
import Help (helpMessage)
import CheckFile (checkFile)

checkOne :: String -> Either String ()
checkOne "-help" = Left helpMessage
checkOne arg | takeExtension arg == ".ink" = Right ()
             | otherwise = Left $ "Unknown argument: " ++ arg

checkArgs :: [String] -> IO (Either String String)
checkArgs [] = return $ Left "No arguments provided. Use -help for usage information."
checkArgs [_ , _] = return $ Left "Too many arguments provided. Use -help for usage information."
checkArgs [arg] =
    case checkOne arg of
        Left err -> return $ Left err
        Right () -> checkFile arg