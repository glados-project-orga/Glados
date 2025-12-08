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
import System.IO (hFlush, stdout, stdin, hIsEOF)
import SExprToAST
import ParserSExpr
import Parser
import Eval
import Data(Ast(..))

writeHelp :: IO ()
writeHelp = putStrLn "LISP interpreter Usage :" >>
            putStrLn "Write in the shell your code (e.g : (define two (1 + 1))." >>
            putStrLn "To quit the interpreter, write exit."

initializeEnv :: IO Env
initializeEnv = mempty

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
startShell env = putStr "> " >> hFlush stdout >>
    hIsEOF stdin >>= \eof ->
        case eof of 
            True -> exitWith ExitSuccess
            False -> getLine >>= \input ->
                case input of
                    "exit" -> exitWith (ExitFailure 84)
                    _ ->
                        case startParser input env of
                            Left err -> putStrLn err >> startShell env
                            Right (newEnv, AVoid) -> startShell newEnv
                            Right (newEnv, ast) -> print ast >> startShell newEnv
