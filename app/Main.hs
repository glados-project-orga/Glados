{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Main
-}

module Main (main) where

import SExprToAST
import ParserSExpr
import Parser
import Eval(evalAST)
import qualified Data.Map as Map

main :: IO ()
main =
    case runParser parseSExpr string of
      Left err -> putStrLn ("Parse error: " ++ err)
      Right (sexpr, _) ->
        case sexprToAST sexpr of
            Left err -> putStrLn ("Could not convert SExpr to AST" ++ err)
            Right ast -> 
              case evalAST Map.empty ast of
                Left err -> putStrLn ("Evaluation error: " ++ err)
                Right result -> putStrLn ("Evaluation result: " ++ show (snd result)) 
    where
      string = "(* 1 1 1)"