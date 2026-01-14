module Class (compileClass) where
import Ast (ClassDecl(..), Declaration(..), FunctionDecl)
import CompilerTypes (CompilerData, Defines)
import Function (compileFunction)
import CompilerTools (appendDefines)
import CompilerError (errPos)

isClassDefined :: String -> Defines -> Bool
isClassDefined searched (_, classDefs, _, _) =
    any (\(ClassDecl _ name _ _) -> name == searched) classDefs

appendMethods :: Either String CompilerData -> [FunctionDecl] -> Either String CompilerData
appendMethods (Left err) _ = Left err
appendMethods prog [] = prog
appendMethods (Right prog) (fun:funs) = appendMethods new_prog funs
    where new_prog = compileFunction fun prog

compileClass :: ClassDecl -> CompilerData -> Either String CompilerData
compileClass def@(ClassDecl pos name _ meth) prog@(_, defs, _, _)
    | isClassDefined name defs = Left ((errPos pos) ++ "Class " ++ name ++ " is already defined.")
    | otherwise = case appendMethods (Right prog) meth of
        Left err -> Left err
        Right meth_progs -> Right (appendDefines meth_progs [(Class def)])