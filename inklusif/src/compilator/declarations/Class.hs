module Class (compileClass) where
import Ast (ClassDecl(..), Declaration(..), FunctionDecl)
import CompilerTypes (CompilerData)
import Function (compileFunction)
import CompilerTools (appendDefines, isClassDefined)
import CompilerError (errPos)

appendMethods :: Either String CompilerData -> [FunctionDecl] -> Either String CompilerData
appendMethods (Left err) _ = Left err
appendMethods prog [] = prog
appendMethods (Right prog) (fun:funs) = appendMethods new_prog True funs
    where new_prog = compileFunction fun prog

compileClass :: ClassDecl -> CompilerData -> Either String CompilerData
compileClass def@(ClassDecl pos name _ meth) prog@(_, defs, _, _)
    | isClassDefined name defs = Left ((errPos pos) ++ "Class " ++ name ++ " is already defined.")
    | otherwise = appendMethods (Right prog) meth >>=
        (\meth_progs -> Right (appendDefines meth_progs [(Class def)]))