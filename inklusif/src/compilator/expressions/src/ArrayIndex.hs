module ArrayIndex (compileArrayIndex) where

import CompilerTypes (CompilerData, Search(..), SearchTypes(..), CompileExpr)
import SymbolTableUtils (getVarIndex, getVarType)
import CompilerTools (getTypePrefix, appendBody, validAssignmentType)

import Ast (ArrayIndexExpr(..), Type(..))

validExprForTask :: CompileExpr -> SearchTypes -> SearchTypes -> CompilerData -> Either String CompilerData
validExprForTask re val sexpr@(SearchExpr expr) prog |
    validAssignmentType val sexpr prog = re expr prog
    | otherwise = Left "Invalid expression type for array index."
validExprForTask _ _ _ _ = Left "Invalid search type for array index."

compileArrayIndex :: CompileExpr -> ArrayIndexExpr -> CompilerData -> Either String CompilerData
compileArrayIndex re (ArrayIndexExpr name index val) prog =
    getVarIndex name prog >>= \varIndex ->
    Right (appendBody prog ["aload " ++ show varIndex]) >>= \loadProg ->
    validExprForTask re (srch IntType) (srch index) loadProg >>= \indexProg ->
    getVarType name indexProg >>= \arrayVal ->
    validExprForTask re (srch arrayVal) (srch val) indexProg >>=
    \valProg -> Right $ appendBody valProg [(getTypePrefix (show arrayVal)) ++ "astore"]