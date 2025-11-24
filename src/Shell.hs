{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Shell
-}

module Shell
    (
        startShell,
        writeHelp
    ) where

import System.Exit
import System.IO (hFlush, stdout)

writeHelp :: IO ()
writeHelp = putStrLn "LISP interpreter Usage :" >>
            putStrLn "Write in the shell your code (e.g : (define two (1 + 1))." >>
            putStrLn "To quit the interpreter, write exit."

startShell :: IO ()
startShell = putStr "> " >> hFlush stdout >>
    getLine >>= \input ->
    case input of
        "exit" -> exitWith (ExitFailure 84)
        _ -> print input >> startShell
