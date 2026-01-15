module ArrayIndex (compileArrayIndex) where

import CompilerTypes (CompilerData)
import SymbolTableUtils (getVarIndex, getVarVal)
import CompilerTools (getTypePrefix, appendBody, validAssignmentType)

import Ast (ArrayIndexExpr(..), Expr(..), Literal(..))
import Expr (compileExpr)

intMacro :: Expr
intMacro = (LitExpr (IntLit 0))

validExprForTask :: Expr -> Expr -> CompilerData -> Either String CompilerData
validExprForTask val expr prog |
    validAssignmentType val expr prog = compileExpr expr prog
    | otherwise = Left "Invalid expression type for array index."

compileArrayIndex :: ArrayIndexExpr -> CompilerData -> Either String CompilerData
compileArrayIndex (ArrayIndexExpr name index val) prog =
    getVarIndex name prog >>= \varIndex ->
    Right (appendBody prog ["aload " ++ show varIndex]) >>= \loadProg ->
    validExprForTask intMacro index loadProg >>= \indexProg ->
    getVarVal name indexProg >>= \arrayVal ->
    validExprForTask (LitExpr(IntLit 0)) val indexProg >>=
    \valProg -> Right $ appendBody valProg [(getTypePrefix (show arrayVal)) ++ "astore"]