{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr args
-}

module ExprArgs (compileArgs) where

import Ast (Expr)
import CompilerTypes (CompilerData, CompileExpr, CompileResult)
import EitherUtils (bindE)

compileArgs :: CompileExpr -> [Expr] -> CompilerData -> CompileResult
compileArgs _ [] prog = Right prog
compileArgs rec (e:es) prog =
  bindE (rec e prog) (\p1 -> compileArgs rec es p1)
