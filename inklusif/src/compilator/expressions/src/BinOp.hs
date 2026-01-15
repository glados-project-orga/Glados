{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- BinOp expression compilation
-}

module BinOp (compileBinOpExpr) where

import Ast (Expr(..), BinOp(..))
import CompilerTypes (CompilerData, CompileExpr, CompilerVal(..))
import CompilerTools (appendBody, convertToCompilerVal)

compileBinOpExpr :: CompileExpr -> Expr -> CompilerData -> Either String CompilerData
compileBinOpExpr compile (BinOpExpr op left right) prog =
    compile left prog >>= \progLeft ->
    compile right progLeft >>= \progRight ->
    convertToCompilerVal left prog >>= \exprType ->
    emitBinOp op exprType progRight
compileBinOpExpr _ _ _ = Left "compileBinOpExpr called with non-BinOp expression"

typePrefix :: CompilerVal -> String
typePrefix (IntCmpl _) = "i"
typePrefix (LongCmpl _) = "l"
typePrefix (FloatCmpl _) = "f"
typePrefix (DoubleCmpl _) = "d"
typePrefix _ = "i"

emitBinOp :: BinOp -> CompilerVal -> CompilerData -> Either String CompilerData
emitBinOp Add t prog = Right $ appendBody prog [typePrefix t ++ "add"]
emitBinOp Sub t prog = Right $ appendBody prog [typePrefix t ++ "sub"]
emitBinOp Mul t prog = Right $ appendBody prog [typePrefix t ++ "mul"]
emitBinOp Div t prog = Right $ appendBody prog [typePrefix t ++ "div"]
emitBinOp Mod t prog = Right $ appendBody prog [typePrefix t ++ "rem"]
emitBinOp op _ _ = Left $ "Op not found: " ++ show op
