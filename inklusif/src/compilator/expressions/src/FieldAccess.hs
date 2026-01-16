module FieldAccess (compileFieldAccess) where

import CompilerTypes (CompilerData)
import Ast (Expr(..), FieldAccessExpr(..))
import SymbolTableUtils (getVarIndex)
import CompilerTools (appendBody)

compileFieldAccess :: FieldAccessExpr -> CompilerData -> Either String CompilerData
compileFieldAccess (FieldAccessExpr (VarExpr varName) fldName) prog = 
    getVarIndex varName prog >>= \compVal ->
    Right (appendBody prog (["aload " ++ show compVal] ++ ["getfield " ++ fldName]))
compileFieldAccess _ _ = Left "Unknown obj call type."