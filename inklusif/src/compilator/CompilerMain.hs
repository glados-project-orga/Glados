module CompilerMain (compilerMain) where
import CompilerTypes (CompilerData, Ast)
import Ast (Declaration(..))
import Function (compileFunction)
import Struct (compileStruct)
import Enum (compileEnum)
import Typedef (compileTypedef)
import CompilerTools (appendDefines)

compileDeclarations :: Declaration -> CompilerData -> Either String CompilerData
compileDeclarations (Function fun) prog = compileFunction fun prog
compileDeclarations (Struct struct) prog = compileStruct struct prog
compileDeclarations (Enum enum) prog = compileEnum enum prog
compileDeclarations (Typedef typedef) prog = compileTypedef typedef prog

completeMainProg :: Either String CompilerData -> Declaration -> Either String CompilerData
completeMainProg (Left err) _ = Left err
completeMainProg (Right incomplete_new_prog) declaration = 
    Right(appendDefines incomplete_new_prog [declaration])

compilerMain :: Ast -> Either String CompilerData -> Either String CompilerData
compilerMain _ (Left err) = Left err
compilerMain [] (Right prog) = Right prog
compilerMain (declaration:ast) (Right prog) = compilerMain ast new_prog
    where
        new_prog = completeMainProg incomplete_new_prog declaration
        incomplete_new_prog = compileDeclarations declaration prog