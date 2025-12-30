{-
-- EPITECH PROJECT, 2025
-- glados-repo
-- File description:
-- Init
-}

module Init
    (
        writeHelp,
        initializeEnv
    ) where

import Eval

writeHelp :: IO ()
writeHelp = putStrLn "LISP interpreter Usage :" >>
            putStrLn "Write in the shell your code (e.g : (define two (+ 1 1))." >>
            putStrLn "To quit the interpreter, write exit."

initializeEnv :: IO Env
initializeEnv = mempty
