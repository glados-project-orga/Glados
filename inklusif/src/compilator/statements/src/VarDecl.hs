module VarDecl (compileVarDecl) where
import CompilerTypes (CompilerData, SymInfo(..))
import CompilerTools (appendSymbolTable, convertToType, typePrefixVal, appendBody, addValToHeader)
import Ast (VarDecl, Type(..), VarDecl(..), Expr(..))
import Expr (compileExpr)

storeVar :: String -> Type -> Expr -> CompilerData -> Either String CompilerData
storeVar name t value prog@(_, _, _, symTable) =
    Right (appendSymbolTable prog [(name, (SymInfo (localindex) t Nothing False))])
    >>= \progWithVar -> compileExpr value progWithVar >>= \valBodyProg ->
    Right (appendBody valBodyProg [typePrefixVal t ++ "store " ++ show (localindex)])
    where localindex = length symTable

storeConstVar :: String -> Type -> Expr -> CompilerData -> Either String CompilerData
storeConstVar name t value prog@(header, _, _, symTable) =
    Right (appendSymbolTable prog [(name, (SymInfo (localindex) t (Just headerindex) False))])
    >>= \progWithVar ->
    addValToHeader progWithVar value
        where localindex = length symTable
              headerindex = length header

isSameType :: Type -> Expr -> CompilerData -> Bool
isSameType t expr prog = case convertToType expr prog of
    Right convertedType -> t == convertedType
    Left _ -> False

compileVarDecl :: VarDecl -> CompilerData -> Either String CompilerData
compileVarDecl (VarDecl _ _ _ True True) _ =
    Left "Cannot have a variable that is both constant and reference."
compileVarDecl (VarDecl name t value True _) prog = storeConstVar name t value prog
compileVarDecl (VarDecl _ _ _ _ True) prog = Right prog
compileVarDecl (VarDecl name t value _ _) prog
    | isSameType t value prog = storeVar name t value prog
    | otherwise = Left "Variable declaration type does not match assigned value type."