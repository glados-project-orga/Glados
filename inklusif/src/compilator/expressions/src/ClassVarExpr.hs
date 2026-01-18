module ClassVarExpr (compileClassVarExpr) where
import Call (compileCallExpr)
import Ast (Expr(..), CallExpr(..))
import CompilerTypes (CompilerData, CompileExpr)
import SymbolTableUtils (getVarIndex)
import CompilerTools (appendBody)

compileFieldAccess :: (String, String) -> CompilerData -> Either String CompilerData
compileFieldAccess (varName, fldName) prog =
    getVarIndex varName prog >>= \compVal ->
    Right (appendBody prog (["aload " ++ show compVal] ++ ["getfield " ++ fldName]))

compileClassVarExpr :: CompileExpr -> (String, Expr) -> CompilerData -> Either String CompilerData
compileClassVarExpr _ (objName, (VarExpr fldName)) prog = compileFieldAccess (objName, fldName) prog
compileClassVarExpr re (objName, (CallExpression (CallExpr cname args))) prog =
    compileCallExpr re (CallExpr cname ((VarExpr objName):args)) prog
compileClassVarExpr _ _ _ = Left "Unknown class variable expression."