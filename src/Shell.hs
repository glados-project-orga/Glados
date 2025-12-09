{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Shell
-}

module Shell
    (
        startShell,
        startParser,
        readAll,
        evalShell
    ) where

import System.Exit
import System.IO (hFlush, stdout, stdin, hIsEOF)
import SExprToAST
import ParserSExpr
import Parser
import Eval
import Data(Ast(..))

evalShell :: String -> Env -> IO ()
evalShell input env =
    case startParser input env of
        Left err -> putStrLn err >> startShell env
        Right (newEnv, AVoid) -> startShell newEnv
        Right (newEnv, ast) -> print ast >> startShell newEnv

readAll :: String -> IO String
readAll buf =
    hIsEOF stdin >>= \eof ->
        case eof of
            True -> return buf
            False -> getLine >>= \input -> readAll (buf ++ input ++ "\n")

startParser :: String -> Env -> Either String (Env, Ast)
startParser str env = case runParser parseSExpr str of
    Left err -> Left ("Parse error: " ++ err)
    Right (sexpr, _) ->
        case sexprToAST sexpr of
            Left err -> Left ("Could not convert SExpr to AST" ++ err)
            Right ast ->
                case evalAST env ast of
                    Left err -> Left err  
                    Right val -> Right val

startShell :: Env -> IO ()
startShell env = putStr "> " >> hFlush stdout >> getLine >>= \input ->
    case input of
        "exit" -> exitWith ExitSuccess
        _ -> evalShell input env
