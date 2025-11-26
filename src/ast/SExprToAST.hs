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

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt mp) = Just (AInt mp)
sexprToAST (SSymbol ll) = Just (ASymbol ll)

sexprToAST (SList [SSymbol "define", SSymbol name, expr]) =
  case sexprToAST expr of
    Just astExpr -> Just (ADefine name astExpr)
    Nothing      -> Nothing

sexprToAST (SList (SSymbol "define" : _)) = Nothing

sexprToAST (SList (fnExpr:args)) = 
  case sexprToAST fnExpr of
    Just fnAst -> case traverse sexprToAST args of
                    Just argAsts -> Just (ACall fnAst argAsts)
                    Nothing      -> Nothing
    Nothing -> Nothing

sexprToAST (SList mpp) =
  case traverse sexprToAST mpp of
    Just asts -> Just (AList asts)
    Nothing   -> Nothing