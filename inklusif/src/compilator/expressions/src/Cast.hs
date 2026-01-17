{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- Cast expression compilation
-}

module Cast (compileCast) where

import Ast (Expr(..), Type(..))
import CompilerTypes (CompilerData, CompileExpr)
import CompilerTools (appendBody, convertToType)

compileCast :: CompileExpr -> Type -> Expr -> CompilerData -> Either String CompilerData
compileCast compile targetType expr prog =
    compile expr prog >>= \exprProg ->
    convertToType expr prog >>= \sourceType ->
    emitConversion sourceType targetType exprProg

emitConversion :: Type -> Type -> CompilerData -> Either String CompilerData

emitConversion src tgt prog | src == tgt = Right prog

emitConversion IntType CharType prog   = Right $ appendBody prog ["i2c"]
emitConversion IntType LongType prog   = Right $ appendBody prog ["i2l"]
emitConversion IntType FloatType prog  = Right $ appendBody prog ["i2f"]
emitConversion IntType DoubleType prog = Right $ appendBody prog ["i2d"]

emitConversion LongType IntType prog    = Right $ appendBody prog ["l2i"]
emitConversion LongType FloatType prog  = Right $ appendBody prog ["l2f"]
emitConversion LongType DoubleType prog = Right $ appendBody prog ["l2d"]

emitConversion FloatType IntType prog    = Right $ appendBody prog ["f2i"]
emitConversion FloatType LongType prog   = Right $ appendBody prog ["f2l"]
emitConversion FloatType DoubleType prog = Right $ appendBody prog ["f2d"]

emitConversion DoubleType IntType prog   = Right $ appendBody prog ["d2i"]
emitConversion DoubleType LongType prog  = Right $ appendBody prog ["d2l"]
emitConversion DoubleType FloatType prog = Right $ appendBody prog ["d2f"]

emitConversion CharType IntType prog = Right $ appendBody prog ["c2i"]

emitConversion BoolType IntType prog = Right prog

emitConversion src tgt _ = Left $ "Unsupported cast"
