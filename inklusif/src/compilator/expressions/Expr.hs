{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Expr (compileExpr) where

import Ast (Expr(..))
import CompilerTypes (CompilerData)

compileExpr :: Expr -> CompilerData -> Either String CompilerData
compileExpr _ prog = Right prog