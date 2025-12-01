{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Shell
-}

module Shell
    (
        startShell,
        writeHelp,
        startParser,
        initializeEnv
    ) where

import System.Exit
import System.IO (hFlush, stdout)
import SExprToAST
import ParserSExpr
import Parser
import Eval

writeHelp :: IO ()
writeHelp = putStrLn "LISP interpreter Usage :" >>
            putStrLn "Write in the shell your code (e.g : (define two (1 + 1))." >>
            putStrLn "To quit the interpreter, write exit."

-- main =
--     case runParser parseSExpr string of
--       Left err -> putStrLn ("Parse error: " ++ err)
--       Right (sexpr, _) ->
--         case sexprToAST sexpr of
--             Left err -> putStrLn ("Could not convert SExpr to AST" ++ err)
--             Right ast -> print ast

initializeEnv :: IO Env
initializeEnv = mempty

startParser :: String -> Env -> IO ()
startParser str env = case runParser parseSExpr str of
    Left err -> putStrLn ("Parse error: " ++ err)
    Right (sexpr, _) ->
        case sexprToAST sexpr of
            Left err -> putStrLn ("Could not convert SExpr to AST" ++ err)
            Right ast -> print $ evalAST env ast

startShell :: Env -> IO ()
startShell env = putStr "> " >> hFlush stdout >>
    getLine >>= \input ->
    case input of
        "exit" -> exitWith (ExitFailure 84)
        _ -> startParser input env >> startShell env
