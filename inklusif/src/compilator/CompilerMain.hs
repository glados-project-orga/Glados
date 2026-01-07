module CompilerMain (compilerMain) where
import CompilerTypes (
    ConstantPool,
    Bytecode,
    Ast,
    ProgramBinary
    )
import Ast (
    Declaration(..),
    FunctionDecl(..),
    StructDecl(..),
    EnumDecl(..),
    TypedefDecl(..)
    )
import Function (compileFunction)
import Struct (compileStruct)
import Enum (compileEnum)
import Typedef (compileTypedef)

compileDeclarations :: Declaration -> ProgramBinary -> ProgramBinary
compileDeclarations (Function fun) prog = compileFunction fun
compileDeclarations (Struct struct) prog = compileStruct struct
compileDeclarations (Enum enum) prog = compileEnum enum
compileDeclarations (Typedef typedef) prog = compileTypedef typedef

compilerMain :: Ast -> ProgramBinary -> ProgramBinary
compilerMain [] prog = prog
compilerMain (declaration:ast) prog = compilerMain ast n_prog
    where
        n_prog = compileDeclarations declaration prog