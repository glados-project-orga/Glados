{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Expr (compileExpr) where

<<<<<<< HEAD
import Ast (Expr(..))
import CompilerTypes (CompilerData)

import Literal (compileLiteralExpr)
import Var (compileVarExpr)
import BinOp (compileBinOpExpr)
import Compile (compileCallExpr)

compileExpr :: Expr -> CompilerData -> Either String CompilerData
compileExpr (LitExpr lit) prog = compileLiteralExpr lit prog
compileExpr (VarExpr name) prog = compileVarExpr name prog
compileExpr (BinOpExpr op a b) prog = compileBinOpExpr compileExpr op a b prog
compileExpr (CallExpression call) prog = compileCallExpr compileExpr call prog
compileExpr _ _ = Left "Expression not implemented yet"
