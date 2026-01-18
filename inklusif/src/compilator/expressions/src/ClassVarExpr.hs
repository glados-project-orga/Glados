module ClassVarExpr (compileClassVarExpr) where
import Call (compileMethExpr)
import Ast (Expr(..), CallExpr(..), ClassAccess(..), Type(..))
import CompilerTypes (CompilerData, CompileExpr)
import SymbolTableUtils (getVarIndex)
import CompilerTools (appendBody, typePrefixVal, convertToType)

compileClassArrayVarExpr :: CompileExpr -> [String] -> Type -> Expr -> CompilerData -> Either String CompilerData
compileClassArrayVarExpr re this t indexExprs prog = re indexExprs prog
    >>= \idxProg  -> Right (appendBody idxProg (this ++[typePrefixVal t ++ "aload" ]))

compileBasicClassAccess :: CompileExpr -> [String] -> (String, ClassAccess) -> Type -> CompilerData -> Either String CompilerData
compileBasicClassAccess re this (_, (ClassMethodCall (CallExpr cname args))) _ prog =
    compileMethExpr re this (CallExpr cname args) prog
compileBasicClassAccess re this (_, (ClassArrayAccess arrName idxExpr)) st prog =
        compileClassArrayVarExpr re (this ++ ["getfield " ++ arrName]) st idxExpr prog
compileBasicClassAccess _ this (_, (ClassVarAccess fldName)) _ prog =
    Right (appendBody prog (this ++["getfield " ++ fldName]))
compileBasicClassAccess re this (_, (ClassClassAccess nclName cacc)) st prog =
    compileBasicClassAccess re (this ++["getfield " ++ nclName]) (nclName, cacc) st prog

compileClassVarExpr :: CompileExpr -> (String, ClassAccess) -> CompilerData -> Either String CompilerData
compileClassVarExpr re clVar@(cname, cacc) prog = getVarIndex cname prog >>=
        \idx -> convertToType (ClassVarExpr cname cacc) prog >>= \subType ->
            compileBasicClassAccess re ["aload " ++ show idx] clVar subType prog