module Function (compileFunction) where
import Ast (FunctionDecl(..))
import Statements (compileStatements)
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import FunctionUtils (searchFunctions)

closeFunction :: Either String CompilerData -> Either String CompilerData
closeFunction (Left err) = Left err
closeFunction (Right prog) = Right (appendBody prog ["}\n"])

compileFunction :: FunctionDecl -> CompilerData-> Either String CompilerData
compileFunction (FunctionDecl _ name _ _ statements) (header, defs, body, _)
    | searchFunctions name defs /= Nothing = Left ("Function " ++ name ++ " is already defined.")
    | otherwise = new_prog
        where new_prog = closeFunction uncomplete_prog
              uncomplete_prog = compileStatements statements (Right open_fun)
              open_fun = (header, defs, body ++ ["fun" ++ " " ++ name ++ " {\n"], [])