module Function (compileFunction) where
import Ast (FunctionDecl(..), Declaration(..), Parameter(..), Statement(..), Type(..))
import Statements (manageBody)
import CompilerTypes (CompilerData)
import CompilerTools (appendBody, appendDefines)
import FunctionUtils (searchFunctions)
import VarDecl (storeInSymbolTable, addGoodTypeStore)
import CompilerError (errPos)


closeFunction :: CompilerData -> FunctionDecl -> Either String CompilerData
closeFunction prog def = Right (appendDefines (appendBody prog ["}"]) [Function def] )

addParams :: [Parameter] -> CompilerData -> Either String CompilerData
addParams [] prog = Right prog
addParams (Parameter name typ _:params) prog =
    addGoodTypeStore typ prog
    >>= \progWithParam -> Right (storeInSymbolTable name typ progWithParam)
    >>= \progWithStore -> addParams params progWithStore


compileFunBody :: [Statement] -> Type -> CompilerData -> Either String CompilerData
compileFunBody statement retype prog =  manageBody statement retype prog

compileFunction :: FunctionDecl -> CompilerData->  Either String CompilerData
compileFunction def@(FunctionDecl pos name params retype statement) prog@(_, defs,_ , _)
    | searchFunctions name defs /= Nothing =
        Left ((errPos pos) ++ "Function " ++ name ++ " is already defined.")
    | otherwise = Right (appendBody prog ["fun" ++ " " ++ name ++ " {"]) >>=
            \progWithOpenFun -> addParams params progWithOpenFun >>=
            \added_params_prog -> compileFunBody statement retype added_params_prog >>=
            \uncomplete_prog -> closeFunction uncomplete_prog def