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
sexprToAST (SSymbol symb) = Right (ASymbol symb)

sexprToAST (SList [SSymbol "define", SSymbol name, expr]) =
  case sexprToAST expr of
    Right astExpr -> Right (ADefine name astExpr)
    Left err      -> Left err

sexprToAST (SList (fnExpr:args)) =
  case sexprToAST fnExpr of
    Right (ASymbol fname) ->
      case traverse sexprToAST args of
        Right argAsts -> Right (ACall (ASymbol fname) argAsts)
        Left err      -> Left err
    Right _ ->
      case traverse sexprToAST (fnExpr:args) of
        Right asts -> Right (AList asts)
        Left err   -> Left err
    Left err -> Left err

sexprToAST _ = Left "Unsupported SExpr form"
