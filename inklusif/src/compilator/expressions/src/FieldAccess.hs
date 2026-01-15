module FieldAccess (compileFieldAccess) where

import CompilerTypes (CompilerData, CompilerVal(..), SymbolTable, Handle)
import Ast (Expr(..), FieldAccessExpr(..))
import SymbolTableUtils (getVarVal)
import CompilerTools (appendBody)

getClassVarHandle :: CompilerVal -> SymbolTable-> Either String Handle
getClassVarHandle (ClassCmpl handle _) _ =  Right handle
getClassVarHandle _ _ = Left "Value is not a class instance."

compileFieldAccess :: FieldAccessExpr -> CompilerData -> Either String CompilerData
compileFieldAccess (FieldAccessExpr (VarExpr varName) fldName) prog@(_, _, _, symTable) = 
    getVarVal varName prog >>= \compVal ->
    getClassVarHandle compVal symTable >>= \varHandle -> 
    Right (appendBody prog (["aload " ++ show varHandle] ++ ["getfield " ++ fldName]))
compileFieldAccess _ _ = Left "Unknown obj call type."