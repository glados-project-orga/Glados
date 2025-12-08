{-
-- EPITECH PROJECT, 2025
-- glados-repo
-- File description:
-- EvalShell
-}

module EvalShell
    (
        splitAux,
        splitSExprs,
        evalAll,
        evalRedirShell
    ) where

import System.Exit
import Eval
import Data(Ast(..))
import Shell

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

evalAll :: [String] -> Env -> IO ()
evalAll [] _ = exitWith ExitSuccess
evalAll (input:expr) env =
    case startParser input env of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right (newEnv, AVoid) -> evalAll expr newEnv
        Right (newEnv, ast) -> print ast >> evalAll expr newEnv

evalRedirShell :: String -> Env -> IO ()
evalRedirShell input env = evalAll (splitSExprs input) env
