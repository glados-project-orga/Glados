{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- UnaryOp expression compilation
-}

module UnaryOp (compileUnaryOp) where

import Ast (Expr(..), UnaryOp(..), Type(..))
import CompilerTypes (CompilerData, CompileExpr)
import CompilerTools (appendBody, convertToType, typePrefixVal)
import SymbolTableUtils (getVarIndex, getVarType)

compileUnaryOp :: CompileExpr -> Expr -> CompilerData -> Either String CompilerData
compileUnaryOp compile (UnaryOpExpr op expr) prog =
    case op of
        Neg   -> compileNeg compile expr prog
        Not   -> compileNot compile expr prog
        PreInc -> compilePreIncDec compile expr 1 prog
        PreDec -> compilePreIncDec compile expr (-1) prog
compileUnaryOp _ _ _ = Left "compileUnaryOp called with non-UnaryOp expression"

compileNeg :: CompileExpr -> Expr -> CompilerData -> Either String CompilerData
compileNeg compile expr prog =
    compile expr prog >>= \exprProg ->
    convertToType expr prog >>= \exprType ->
    emitNeg exprType exprProg

emitNeg :: Type -> CompilerData -> Either String CompilerData
emitNeg IntType prog    = Right $ appendBody prog ["ineg"]
emitNeg LongType prog   = Right $ appendBody prog ["lneg"]
emitNeg FloatType prog  = Right $ appendBody prog ["fneg"]
emitNeg DoubleType prog = Right $ appendBody prog ["dneg"]
emitNeg t _ = Left $ "Negation not supported for type: " ++ show t

compileNot :: CompileExpr -> Expr -> CompilerData -> Either String CompilerData
compileNot compile expr prog =
    compile expr prog >>= \exprProg ->
    convertToType expr prog >>= \exprType ->
    emitNot exprType exprProg

emitNot :: Type -> CompilerData -> Either String CompilerData
emitNot BoolType prog = Right $ appendBody prog ["iconst 1", "ixor"]
emitNot IntType prog  = Right $ appendBody prog ["iconst 1", "ixor"]
emitNot t _ = Left $ "not not supported for type: " ++ show t

compilePreIncDec :: CompileExpr -> Expr -> Int -> CompilerData -> Either String CompilerData
compilePreIncDec _ (VarExpr varName) sign prog =
    getVarIndex varName prog >>= \idx ->
    getVarType varName prog >>= \varType ->
    let prefix = typePrefixVal varType
        loadInstr = prefix ++ "load " ++ show idx
        storeInstr = prefix ++ "store " ++ show idx
        constInstr = prefix ++ "const " ++ show (abs sign)
        opInstr = if sign > 0
                  then prefix ++ "add"
                  else prefix ++ "sub"
    in Right $ appendBody prog [loadInstr, constInstr, opInstr, "dup", storeInstr]
compilePreIncDec _ _ _ _ = Left "Pre-increment/decrement needs a variable"
