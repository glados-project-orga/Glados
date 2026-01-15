{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- BinOp expression compilation
-}

module BinOp (compileBinOpExpr) where

import Ast (Expr(..), BinOp(..))
import CompilerTypes (CompilerData, CompileExpr, CompilerVal(..))
import CompilerTools (appendBody, convertToCompilerVal, typePrefixVal)
import SymbolTableUtils (getVarIndex, getVarVal)

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
             convertToCompilerVal left prog >>= \exprType ->
             emitBinOp op exprType progRight
compileBinOpExpr _ _ _ = Left "compileBinOpExpr called with non-BinOp expression"

compileCompoundAssign :: CompileExpr -> BinOp -> Expr -> Expr -> CompilerData -> Either String CompilerData
compileCompoundAssign compile op (VarExpr varName) right prog =
    getVarIndex varName prog >>= \idx ->
    getVarVal varName prog >>= \varType ->
    let loadInstr = typePrefixVal varType ++ "load " ++ show idx
        storeInstr = typePrefixVal varType ++ "store " ++ show idx
    in Right (appendBody prog [loadInstr]) >>= \progLoad ->
       compile right progLoad >>= \progRight ->
       emitBinOp op varType progRight >>= \progOp ->
       Right (appendBody progOp [storeInstr])
compileCompoundAssign _ _ _ _ _ =
    Left "Compound assignment requires a variable on the left side"

emitBinOp :: BinOp -> CompilerVal -> CompilerData -> Either String CompilerData

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

emitComparison :: CompilerVal -> String -> CompilerData -> CompilerData
emitComparison (IntCmpl _) cond prog = emitIntComparison cond prog
emitComparison t cond prog = emitCmpThenBranch t cond prog

emitIntComparison :: String -> CompilerData -> CompilerData
emitIntComparison cond prog = appendBody prog
    [ "if_icmp" ++ cond ++ " 3"
    , "iconst 0"
    , "goto 2"
    , "iconst 1"
    ]

emitCmpThenBranch :: CompilerVal -> String -> CompilerData -> CompilerData
emitCmpThenBranch t cond prog = appendBody prog
    [ cmpInstr t
    , "if" ++ cond ++ " 3"
    , "iconst 0"
    , "goto 2"
    , "iconst 1"
    ]

cmpInstr :: CompilerVal -> String
cmpInstr (LongCmpl _) = "lcmp"
cmpInstr (FloatCmpl _) = "fcmpl"
cmpInstr (DoubleCmpl _) = "dcmpl"
cmpInstr _ = "lcmp"
