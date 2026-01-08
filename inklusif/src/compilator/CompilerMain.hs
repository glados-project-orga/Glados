module CompilerMain (compilerMain) where
import CompilerTypes (ProgramBinary, Ast)
import Ast (Declaration(..))
import Function (compileFunction)
import Struct (compileStruct)
import Enum (compileEnum)
import Typedef (compileTypedef)
import CompilerTools (appendDefines)

compileDeclarations :: Declaration -> ProgramBinary -> ProgramBinary
compileDeclarations (Function fun) prog = compileFunction fun prog
compileDeclarations (Struct struct) prog = compileStruct struct prog
compileDeclarations (Enum enum) prog = compileEnum enum prog
compileDeclarations (Typedef typedef) prog = compileTypedef typedef prog

compilerMain :: Ast -> ProgramBinary -> ProgramBinary
compilerMain [] prog = prog
compilerMain (declaration:ast) prog = compilerMain ast new_prog
    where
        new_prog = appendDefines incomplete_new_prog [declaration]
        incomplete_new_prog = compileDeclarations declaration prog