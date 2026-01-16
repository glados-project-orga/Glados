module ArrayIndex (compileArrayIndex) where

import CompilerTypes (CompilerData, Search(..), SearchTypes(..))
import SymbolTableUtils (getVarIndex, getVarType)
import CompilerTools (getTypePrefix, appendBody, validAssignmentType)

import Ast (ArrayIndexExpr(..), Type(..))
import Expr (compileExpr)

validExprForTask :: SearchTypes -> SearchTypes -> CompilerData -> Either String CompilerData
validExprForTask val sexpr@(SearchExpr expr) prog |
    validAssignmentType val sexpr prog = compileExpr expr prog
    | otherwise = Left "Invalid expression type for array index."
validExprForTask _ _ _ = Left "Invalid search type for array index."

compileArrayIndex :: ArrayIndexExpr -> CompilerData -> Either String CompilerData
compileArrayIndex (ArrayIndexExpr name index val) prog =
    getVarIndex name prog >>= \varIndex ->
    Right (appendBody prog ["aload " ++ show varIndex]) >>= \loadProg ->
    validExprForTask (srch IntType) (srch index) loadProg >>= \indexProg ->
    getVarType name indexProg >>= \arrayVal ->
    validExprForTask (srch arrayVal) (srch val) indexProg >>=
    \valProg -> Right $ appendBody valProg [(getTypePrefix (show arrayVal)) ++ "astore"]