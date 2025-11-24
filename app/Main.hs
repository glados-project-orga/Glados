{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Main
-}

module Main (main) where

import Ast.LispAst
import ParserSExpr
import Parser

main :: IO ()
main =
    case runParser parseSExpr string of
      Left err -> putStrLn ("Parse error: " ++ err)
      Right (sexpr, _) ->
        case sexprToAST sexpr of
            Nothing -> putStrLn "Could not convert SExpr to AST"
            Just ast -> print ast
    where
      string = "(1 \"hello\" 1)"