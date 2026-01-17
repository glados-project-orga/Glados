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

noArgs :: String 
noArgs = "No arguments provided. Use -help for usage information."

tooMuchArgs :: String 
tooMuchArgs = "Too many arguments provided. Use -help for usage information."

checkOne :: String -> Either String ()
checkOne "-help" = Left helpMessage
checkOne arg | takeExtension arg == ".ink" = Right ()
             | otherwise = Left $ "Unknown argument: " ++ arg

checkArgs :: [String] -> IO (Either String String)
checkArgs [] = return $ Left noArgs
checkArgs [arg] =
    case checkOne arg of
        Left err -> return $ Left err
        Right () -> checkFile arg
checkArgs _ = return $ Left tooMuchArgs
