module VarDecl (compileVarDecl, storeInSymbolTable, addGoodTypeStore) where
import CompilerTypes (CompilerData, SymInfo(..), TypeEq(..))
import CompilerTools (appendSymbolTable, convertToType, typePrefixVal, appendBody, addValToHeader)
import ArrayLiteral (compileArrayLiteral)
import Ast (VarDecl, Type(..), VarDecl(..), Expr(..), ArrayVar(..))
import Expr (compileExpr)

storeInSymbolTable :: String -> Type -> CompilerData -> CompilerData
storeInSymbolTable name t prog@(_, _, _, symTable) =
    appendSymbolTable prog [(name, (SymInfo (localindex) t Nothing False))]
        where localindex = length symTable

addGoodTypeStore :: Type -> CompilerData -> Either String CompilerData
addGoodTypeStore t prog@(_, _, _, symTable) =
    Right (appendBody prog [typePrefixVal t ++ "store " ++ show (localindex)])
        where localindex = length symTable

storeVar :: String -> Type -> Expr -> CompilerData -> Either String CompilerData
storeVar name t value prog =
    compileExpr value prog >>= \expryProg ->
    addGoodTypeStore t expryProg >>= \valBodyProg ->
    Right(storeInSymbolTable name t valBodyProg)

storeConstVar :: String -> Type -> Expr -> CompilerData -> Either String CompilerData
storeConstVar name t value prog@(header, _, _, symTable) =
    Right (appendSymbolTable prog [(name, (SymInfo (localindex) t (Just headerindex) False))])
    >>= \progWithVar ->
    addValToHeader progWithVar value
        where localindex = length symTable
              headerindex = length header

storeArrayVar :: String -> Type -> ArrayVar -> Expr -> CompilerData -> Either String CompilerData
storeArrayVar name t (ArrayVar at len) (ArrayLiteral exprs) prog =
             compileExpr len prog
            >>= \lenProg -> Right (appendBody lenProg ["newarray " ++ show at])
            >>= \newArrayProg -> compileArrayLiteral compileExpr exprs at newArrayProg
            >>= \newArrayWithElementsProg -> addGoodTypeStore t newArrayWithElementsProg
            >>= \storedProg -> Right (storeInSymbolTable name t storedProg)
storeArrayVar _ _ _ expr _ = Left ("Invalid array variable declaration with. " ++ show expr)

isSameType :: Type -> Expr -> CompilerData -> Bool
isSameType t expr prog = case convertToType expr prog of
    Right convertedType -> t `typeEq` convertedType
    Left _ -> False

compileVarDecl :: VarDecl -> CompilerData -> Either String CompilerData
compileVarDecl (VarDecl _ _ _ True True) _ =
    Left "Cannot have a variable that is both constant and reference."
compileVarDecl (VarDecl name t value True _) prog = storeConstVar name t value prog
compileVarDecl (VarDecl _ _ _ _ True) prog = Right prog
compileVarDecl (VarDecl name t value _ _) prog
    | isSameType t value prog = case t of
        ArrayType arr -> storeArrayVar name t arr value prog
        CustomType cname -> Right (appendBody prog ["new " ++ cname]) >>= \newArrayProg ->
            Right (storeInSymbolTable name t newArrayProg)
        _ -> storeVar name t value prog
    | otherwise = Left ("Variable declaration type does not match assigned value type." ++ 
        case convertToType value prog of
            Right ct -> " Expected " ++ show t ++ ", got " ++ show ct ++ "."
            Left err -> " (Could not determine type: " ++ err ++ ")")