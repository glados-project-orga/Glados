{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- BinOp expression compilation
-}

module BinOp (compileBinOpExpr) where

import Ast (Expr(..), BinOp(..))
import CompilerTypes (CompilerData, CompileExpr)
import CompilerTools (appendBody)

compileBinOpExpr :: CompileExpr -> Expr -> CompilerData -> Either String CompilerData
compileBinOpExpr compile (BinOpExpr op left right) prog =
    compile left prog >>= \progLeft ->
    compile right progLeft >>= \progRight ->
    emitBinOp op progRight
compileBinOpExpr _ _ _ = Left "compileBinOpExpr called with non-BinOp expression"

emitBinOp :: BinOp -> CompilerData -> Either String CompilerData
emitBinOp Add prog = Right (appendBody prog ["iadd"])
emitBinOp Sub prog = Right (appendBody prog ["isub"])
emitBinOp Mul prog = Right (appendBody prog ["imul"])
emitBinOp Div prog = Right (appendBody prog ["idiv"])
emitBinOp Mod prog = Right (appendBody prog ["irem"])
emitBinOp op _ = Left ("BinOp not implemented yet: " ++ show op)
