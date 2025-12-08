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
        evalShell, 
        startParser,
        evalRedirShell,
        readAll,
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
            putStrLn "Write in the shell your code (e.g : (define two (+ 1 1))." >>
            putStrLn "To quit the interpreter, write exit."

initializeEnv :: IO Env
initializeEnv = mempty

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

splitSExprs :: String -> [String]
splitSExprs str = splitAux str 0 "" []

splitAux :: String -> Int -> String -> [String] -> [String]
splitAux [] _ "" acc = reverse acc
splitAux [] _ buf acc = reverse (buf:acc)
splitAux (c:cs) nbPar buf acc
    | c == '('  = splitAux cs (nbPar + 1) (buf ++ [c]) acc
    | c == ')'  = if (nbPar - 1) == 0
        then splitAux cs 0 "" ((buf ++ [c]) : acc)
        else splitAux cs (nbPar - 1) (buf ++ [c]) acc
    | nbPar == 0 && c == '\n' && not (null buf) =
                    splitAux cs nbPar "" ((buf ++ [c]):acc)
    | c == '\n' || c == '\t' = splitAux cs nbPar buf acc
    | otherwise = splitAux cs nbPar (buf ++ [c]) acc


printExprs :: [String] -> IO ()
printExprs [] = exitWith ExitSuccess
printExprs (input:expr) = putStrLn input >> printExprs expr

evalRedirShell :: String -> Env -> IO ()
evalRedirShell input env = evalAll (splitSExprs input) env

evalAll :: [String] -> Env -> IO ()
evalAll [] _ = exitWith ExitSuccess
evalAll (input:expr) env =
    case startParser input env of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right (newEnv, AVoid) -> evalAll expr newEnv
        Right (newEnv, ast) -> print ast >> evalAll expr newEnv

evalShell :: String -> Env -> IO ()
evalShell input env =
    case startParser input env of
        Left err -> putStrLn err >> startShell env
        Right (newEnv, AVoid) -> startShell newEnv
        Right (newEnv, ast) -> print ast >> startShell newEnv

startShell :: Env -> IO ()
startShell env = putStr "> " >> hFlush stdout >> getLine >>= \input ->
    case input of
        "exit" -> exitWith (ExitFailure 84)
        _ -> evalShell input env
