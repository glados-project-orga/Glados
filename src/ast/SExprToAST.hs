{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- SExprToAST.hs
-}

module SExprToAST (
  sexprToAST
) where

import Data.Traversable()
import Data.Maybe()
import Data(Ast(..), SExpr(..))

sexprToAST :: SExpr -> Either String Ast
sexprToAST (SInt mp) = Right (AInt mp)
sexprToAST (SSymbol symb) = case symb of
  "#f" -> Right (ABool False)
  "#t" -> Right (ABool True)
  str -> Right (ASymbol str)

sexprToAST (SList [SSymbol "define", SSymbol name, expr]) =
  case sexprToAST expr of
    Right astExpr -> Right (ADefine name astExpr)
    Left err      -> Left err

sexprToAST (SList [SSymbol "define", SList (SSymbol name : params), body]) =
  case traverse sexprToAST params of
    Right argAsts ->
      let argNames = [ n | ASymbol n <- argAsts ]
      in case sexprToAST body of
          Right bdy -> Right (ADefine name (ALambda argNames bdy))
          Left err  -> Left err
    Left err -> Left err

sexprToAST (SList [SSymbol "lambda", SList params, body]) =
  case traverse sexprToAST params of
    Right argAsts ->
      let argNames = [ name | ASymbol name <- argAsts]
      in case sexprToAST body of
          Right bdy-> Right (ALambda argNames bdy)
          Left err -> Left err
    Left err -> Left err

sexprToAST (SList (fnExpr:args)) =
  case sexprToAST fnExpr of
    Right (ASymbol fname) ->
      case traverse sexprToAST args of
        Right argAsts -> Right (ACall (ASymbol fname) argAsts)
        Left err      -> Left err
    Right (ALambda params body) ->
      case traverse sexprToAST args of
        Right argAsts -> Right (ACall (ALambda params body) argAsts)
        Left err      -> Left err
    Right _ ->
      case traverse sexprToAST (fnExpr:args) of
        Right asts -> Right (AList asts)
        Left err   -> Left err
    Left err -> Left err

sexprToAST _ = Left "Unsupported SExpr form"
