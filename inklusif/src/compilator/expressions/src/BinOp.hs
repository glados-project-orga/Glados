{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module BinOp (compileBinOpExpr) where

import Ast (Expr(..), BinOp(..))
import CompilerTypes (CompilerData, CompileExpr, CompileResult)
import CompilerTools (appendBody)
import EitherUtils (bindE)

compileBinOpExpr :: Expr -> CompilerData -> Either String CompilerData
compileBinOpExpr (BinOpExpr op left right) prog = Left "Placeholder"
compileBinOpExpr _ _ = Left "Expected BinOpExpr in compileBinOpExpr"

