module VarDecl (compileVarDecl) where
import CompilerTypes (CompilerData, SymInfo(..))
import CompilerTools (appendSymbolTable, convertToType, typePrefixVal, appendBody)
import Ast (VarDecl, Type(..), VarDecl(..), Expr(..))
import Expr (compileExpr)

storeVar :: String -> Type -> Expr -> CompilerData -> Either String CompilerData
storeVar name t value prog@(_, _, _, symTable) =
    Right (appendSymbolTable prog [(name, (SymInfo (localindex) t))]) >>= \progWithVar ->
    compileExpr value progWithVar >>= \valProg ->
    Right (appendBody valProg [typePrefixVal t ++ "store " ++ show (localindex)])
    where localindex = length symTable

isSameType :: Type -> Expr -> CompilerData -> Bool
isSameType t expr prog = case convertToType expr prog of
    Right convertedType -> t == convertedType
    Left _ -> False

compileVarDecl :: VarDecl -> CompilerData -> Either String CompilerData
compileVarDecl (VarDecl _ _ _ True True) _ =
    Left "Cannot have a variable that is both reference and constant."
compileVarDecl (VarDecl _ _ _ True _) prog = Right prog
compileVarDecl (VarDecl _ _ _ _ True) prog = Right prog
compileVarDecl (VarDecl name t value _ _) prog
    | isSameType t value prog = storeVar name t value prog
    | otherwise = Left "Variable declaration type does not match assigned value type."