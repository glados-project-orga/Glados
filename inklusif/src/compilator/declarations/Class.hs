module Class (compileClass) where
import Ast (ClassDecl(..), Declaration(..), FunctionDecl(..), Parameter(..), Type(..))
import CompilerTypes (CompilerData)
import Function (compileFunction)
import CompilerTools (appendDefines, isClassDefined)
import CompilerError (errPos)

appendMethods :: Either String CompilerData -> String-> [FunctionDecl] -> Either String CompilerData
appendMethods (Left err) _ _ = Left err
appendMethods prog _ [] = prog
appendMethods (Right prog) cname ((FunctionDecl pos name params retype statement):funs) =
    appendMethods new_prog cname funs
    where new_prog = compileFunction updated_fun prog
          updated_fun = FunctionDecl pos name
            ((Parameter "this" (CustomType cname) False):params)
            retype statement

compileClass :: ClassDecl -> CompilerData -> Either String CompilerData
compileClass def@(ClassDecl pos name _ meth) prog@(_, defs, _, _)
    | isClassDefined name defs = Left ((errPos pos) ++ "Class " ++ name ++ " is already defined.")
    | otherwise =  Right (appendDefines prog [(Class def)]) >>=
        \defProg -> appendMethods (Right defProg) name meth