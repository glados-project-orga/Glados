{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Main
-}

module Main (main) where

import Shell
import Parser

main :: IO ()
main = (writeHelp >> initializeEnv) >>= \env -> 
  startShell env

-- import SExprToAST
-- import ParserSExpr
-- import Parser
-- 
-- main :: IO ()
-- main =
--     case runParser parseSExpr string of
--       Left err -> putStrLn ("Parse error: " ++ err)
--       Right (sexpr, _) ->
--         case sexprToAST sexpr of
--             Left err -> putStrLn ("Could not convert SExpr to AST" ++ err)
--             Right ast -> print ast
--     where
--       string = "(1 \"hello\" 1)"
