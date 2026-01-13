{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Expr (compileExpr, compileExprT) where

import Ast (Expr(..), Type(..))
import CompilerTypes (CompilerData)
import Literal (compileLiteralExprT)
import BinOp (compileBinOpExprT)
import Call (compileCallExprT)
import Var (compileVarExprT)

compileExpr :: Expr -> CompilerData -> Either String CompilerData
compileExpr e prog = snd <$> compileExprT e prog

compileExprT :: Expr -> CompilerData -> Either String (Type, CompilerData)
compileExprT (LitExpr lit) prog = compileLiteralExprT lit prog
compileExprT (VarExpr name) prog = compileVarExprT name prog
compileExprT (BinOpExpr op a b) prog = compileBinOpExprT compileExprT op a b prog
compileExprT (CallExpression call) prog = compileCallExprT compileExprT call prog
compileExprT _ _ = Left "expression not implemented yet"