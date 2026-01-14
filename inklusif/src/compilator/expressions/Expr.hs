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

compileExpr :: Expr -> CompilerData -> Either String CompilerData
compileExpr (LitExpr lit) prog = compileLiteralExpr lit prog
compileExpr _ _ = Left "Expression type not implemented yet"