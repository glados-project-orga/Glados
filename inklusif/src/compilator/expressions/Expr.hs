{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Expr (compileExpr) where

import Ast (Expr(..))
import CompilerTypes (CompilerData)
import Literal (compileLiteralExpr)
import BinOp (compileBinOpExpr)


compileExpr :: Expr -> CompilerData -> Either String CompilerData
compileExpr (LitExpr lit) prog = compileLiteralExpr lit prog
compileExpr expr@(BinOpExpr _ _ _) prog = compileBinOpExpr compileExpr expr prog
compileExpr _ _ = Left "Expression type not implemented yet"
