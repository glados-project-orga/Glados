{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- BinOp expression compilation
-}

module BinOp (compileBinOpExpr) where

import Labels (generateLabel)
import Ast (Expr(..), BinOp(..), Type(..))
import CompilerTypes (CompilerData, CompileExpr)
import CompilerTools (appendBody, convertToType, typePrefixVal)
import SymbolTableUtils (getVarIndex, getVarType)

compileBinOpExpr :: CompileExpr -> Expr -> CompilerData -> Either String CompilerData
compileBinOpExpr compile (BinOpExpr op left right) prog =
    case op of
        AddEqual -> compileCompoundAssign compile Add left right prog
        SubEqual -> compileCompoundAssign compile Sub left right prog
        MulEqual -> compileCompoundAssign compile Mul left right prog
        DivEqual -> compileCompoundAssign compile Div left right prog
        ModEqual -> compileCompoundAssign compile Mod left right prog
        _ -> compile left prog >>= \progLeft ->
             compile right progLeft >>= \progRight ->
             convertToType left prog >>= \exprType ->
             emitBinOp op exprType progRight
compileBinOpExpr _ _ _ = Left "compileBinOpExpr called with non-BinOp expression"

compileCompoundAssign :: CompileExpr -> BinOp -> Expr -> Expr -> CompilerData -> Either String CompilerData
compileCompoundAssign compile op (VarExpr varName) right prog =
    getVarIndex varName prog >>= \idx ->
    getVarType varName prog >>= \varType ->
    let loadInstr = typePrefixVal varType ++ "load " ++ show idx
        storeInstr = typePrefixVal varType ++ "store " ++ show idx
    in Right (appendBody prog [loadInstr]) >>= \progLoad ->
       compile right progLoad >>= \progRight ->
       emitBinOp op varType progRight >>= \progOp ->
       Right (appendBody progOp [storeInstr])
compileCompoundAssign _ _ _ _ _ =
    Left "Compound assignment requires a variable on the left side"

emitBinOp :: BinOp -> Type -> CompilerData -> Either String CompilerData

emitBinOp Add t prog = Right $ appendBody prog [typePrefixVal t ++ "add"]
emitBinOp Sub t prog = Right $ appendBody prog [typePrefixVal t ++ "sub"]
emitBinOp Mul t prog = Right $ appendBody prog [typePrefixVal t ++ "mul"]
emitBinOp Div t prog = Right $ appendBody prog [typePrefixVal t ++ "div"]
emitBinOp Mod t prog = Right $ appendBody prog [typePrefixVal t ++ "rem"]

emitBinOp Equal t prog = Right $ emitComparison t "eq" prog
emitBinOp NotEqual t prog = Right $ emitComparison t "ne" prog
emitBinOp LessThan t prog = Right $ emitComparison t "lt" prog
emitBinOp GreaterThan t prog = Right $ emitComparison t "gt" prog
emitBinOp LessEqual t prog = Right $ emitComparison t "le" prog
emitBinOp GreaterEqual t prog = Right $ emitComparison t "ge" prog

emitBinOp And _ prog = Right $ appendBody prog ["iand"]
emitBinOp Or _ prog = Right $ appendBody prog ["ior"]

emitBinOp op _ _ = Left $ "BinOp not implemented: " ++ show op

emitComparison :: Type -> String -> CompilerData -> CompilerData
emitComparison (IntType) cond prog = emitIntComparison (IntType) cond prog
emitComparison t cond prog = emitCmpThenBranch t cond prog

emitIntComparison :: Type -> String -> CompilerData -> CompilerData
emitIntComparison t cond prog =
    let (lTrue,  prog1) = generateLabel prog
        (lEnd,   prog2) = generateLabel prog1
        prog3 = appendBody prog2 ["if_icmp" ++ cond ++ " " ++ lTrue]
        prog4 = appendBody prog3 ["iconst 0"]
        prog5 = appendBody prog4 ["goto " ++ lEnd]
        prog6 = appendBody prog5 [lTrue ++ ":"]
        prog7 = appendBody prog6 ["iconst 1"]
        prog8 = appendBody prog7 [lEnd ++ ":"]
    in prog8


emitCmpThenBranch :: Type -> String -> CompilerData -> CompilerData
emitCmpThenBranch t cond prog =
    let (lTrue,  prog1) = generateLabel prog
        (lEnd,   prog2) = generateLabel prog1
        prog3 = appendBody prog2 [cmpInstr t]
        prog4 = appendBody prog3 ["if" ++ cond ++ " " ++ lTrue]
        prog5 = appendBody prog4 ["iconst 0"]
        prog6 = appendBody prog5 ["goto " ++ lEnd]
        prog7 = appendBody prog6 [lTrue ++ ":"]
        prog8 = appendBody prog7 ["iconst 1"]
        prog9 = appendBody prog8 [lEnd ++ ":"]
    in prog9


cmpInstr :: Type -> String
cmpInstr (LongType) = "lcmp"
cmpInstr (FloatType) = "fcmpl"
cmpInstr (DoubleType) = "dcmpl"
cmpInstr _ = "lcmp"
