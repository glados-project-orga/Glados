module ArrayIndex (compileArrayIndex) where

import CompilerTypes (CompilerData, CompilerVal(..))
import SymbolTableUtils (getVarIndex, getVarVal)
import CompilerTools (getTypePrefix, appendBody, validAssignmentType)

import Ast (ArrayIndexExpr(..), Expr(..))
import Expr (compileExpr)

validExprForTask :: CompilerVal -> Expr -> CompilerData -> Either String CompilerData
validExprForTask val expr prog |
    validAssignmentType val expr prog = compileExpr expr prog
    | otherwise = Left "Invalid expression type for array index."

compileArrayIndex :: ArrayIndexExpr -> CompilerData -> Either String CompilerData
compileArrayIndex (ArrayIndexExpr name index val) prog =
    getVarIndex name prog >>= \varIndex ->
    Right (appendBody prog ["aload " ++ show varIndex]) >>= \loadProg ->
    validExprForTask (IntCmpl 0) index loadProg >>= \indexProg ->
    getVarVal name indexProg >>= \arrayVal ->
    validExprForTask arrayVal val indexProg >>=
    \valProg -> Right $ appendBody valProg [(getTypePrefix (show arrayVal)) ++ "astore"]