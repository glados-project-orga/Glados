module Function (compileFunction) where
import Ast (FunctionDecl(..), Declaration(..))
import Statements (compileStatements)
import CompilerTypes (CompilerData)
import CompilerTools (appendBody, appendDefines)
import FunctionUtils (searchFunctions)
import CompilerError (errPos)

closeFunction :: Either String CompilerData -> FunctionDecl -> Either String CompilerData
closeFunction (Left err) _ = Left err
closeFunction (Right prog) def = Right (appendDefines (appendBody prog ["}\n"]) [Function def] )

-- addParams :: [ParamDecl] -> CompilerData -> CompilerData
-- addParams [] prog = prog
-- addParams (ParamDecl name typ True:params) prog =

compileFunction :: FunctionDecl -> CompilerData-> Either String CompilerData
compileFunction def@(FunctionDecl pos name _ _ statements) (header, defs, body, _)
    | searchFunctions name defs /= Nothing =
        Left ((errPos pos) ++ "Function " ++ name ++ " is already defined.")
    | otherwise = new_prog
        where new_prog = closeFunction uncomplete_prog def
              uncomplete_prog = compileStatements statements (Right open_fun)
              open_fun = (header, defs, body ++ ["fun" ++ " " ++ name ++ " {\n"], [])