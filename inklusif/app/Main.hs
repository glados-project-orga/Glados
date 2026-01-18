{-
-- EPITECH PROJECT, 2025
-- glados-repo
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import Help (argErrorLog, parsingErrorLog)
import Args (checkArgs)
import InkParser (parseInkFile)
import CompilerMain (compileAst)

main :: IO ()
main = getArgs >>= \args ->
    checkArgs args >>= either argErrorLog (\content -> 
        (putStrLn $ "File content loaded successfully:\n" ++ content) >>
            parseInkFile content >>=
                either parsingErrorLog (\decl ->
                    case compileAst decl of
                        Left err -> putStrLn $ "Compilation error: " ++ err
                        Right ioAction -> ioAction
                ))