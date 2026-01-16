{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Expr (compileExpr) where

import Ast (Expr(..), Type(..), ArrayVar(..))
import CompilerTypes (CompilerData, CompileExpr)
import CompilerTools (getLitArrayType)
import CompileLiteral (compileLiteralExpr)
import BinOp (compileBinOpExpr)
import ArrayIndex (compileArrayIndex)
import ArrayLiteral (compileArrayLiteral)
import Call (compileCallExpr)
import MethodCall (compileMethodCall)
import FieldAccess (compileFieldAccess)
import Var (compileVarExpr)


prepareArrayLiteral :: CompileExpr -> [Expr] -> CompilerData -> Either String CompilerData
prepareArrayLiteral _ [] prog = Right prog
prepareArrayLiteral re exprs prog = arrayType >>= \at -> compileArrayLiteral re exprs at prog
    where arrayType = getLitArrayType exprs prog >>= \t -> case t of
            (ArrayType (ArrayVar at _)) -> Right at
            _ -> Left "Expected array type"

compileExpr :: Expr -> CompilerData -> Either String CompilerData
compileExpr (LitExpr lit) prog = compileLiteralExpr lit prog
compileExpr (ArrayAssignement indx) prog = compileArrayIndex compileExpr indx prog
compileExpr (ArrayLiteral exprs) prog = prepareArrayLiteral compileExpr exprs prog
compileExpr (CallExpression call) prog = compileCallExpr compileExpr call prog
compileExpr (MethodCallExpression objCall) prog = compileMethodCall compileExpr objCall prog
compileExpr (FieldAccessExpression fieldAccess) prog = compileFieldAccess fieldAccess prog
compileExpr (VarExpr expr) prog = compileVarExpr expr prog
compileExpr expr@(BinOpExpr _ _ _) prog = compileBinOpExpr compileExpr expr prog
compileExpr _ _ = Left "Expression type not implemented yet"
