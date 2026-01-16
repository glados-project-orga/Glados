module Function (compileFunction) where
import Ast (FunctionDecl(..), Declaration(..), Parameter(..), Statement(..), ReturnStmt(..), Type(..))
import Statements (compileStatements)
import CompilerTypes (CompilerData, Search(..))
import CompilerTools (appendBody, appendDefines, validAssignmentType)
import FunctionUtils (searchFunctions)
import VarDecl (storeInSymbolTable, addGoodTypeStore)
import CompilerError (errPos)


closeFunction :: CompilerData -> FunctionDecl -> Either String CompilerData
closeFunction prog def = Right (appendDefines (appendBody prog ["}"]) [Function def] )

addParams :: [Parameter] -> CompilerData -> Either String CompilerData
addParams [] prog = Right prog
addParams (Parameter name typ _:params) prog = Right (storeInSymbolTable name typ prog)
    >>= \progWithParam -> addGoodTypeStore typ progWithParam
    >>= \progWithStore -> addParams params progWithStore

checkReturnType :: [Statement] -> Type -> CompilerData -> Either String CompilerData
checkReturnType [] VoidType prog = Right (prog)
checkReturnType [] _ _ = Left "Function missing return statement in non-void function."
checkReturnType ((ReturnStatement (ReturnStmt expr)):_) retype prog
    | validAssignmentType (srch retype) (srch expr) prog = Right (prog)
    | otherwise = Left "Invalid return Type in function."
checkReturnType (_:stmts) retype prog = checkReturnType stmts retype prog


compileFunBody :: [Statement] -> Type -> CompilerData -> Either String CompilerData
compileFunBody statement retype prog = checkReturnType statement retype prog
    >>= \checkedProg -> compileStatements statement (Right checkedProg)

compileFunction :: FunctionDecl -> CompilerData-> Either String CompilerData
compileFunction def@(FunctionDecl pos name params retype statement) prog@(_, defs,_ , _)
    | searchFunctions name defs /= Nothing =
        Left ((errPos pos) ++ "Function " ++ name ++ " is already defined.")
    | otherwise = Right (appendBody prog ["fun" ++ " " ++ name ++ " {"]) >>=
            \progWithOpenFun -> addParams params progWithOpenFun >>=
            \added_params_prog -> compileFunBody statement retype added_params_prog >>=
            \uncomplete_prog -> closeFunction uncomplete_prog def