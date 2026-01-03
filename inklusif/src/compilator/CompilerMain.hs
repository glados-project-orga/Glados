module CompilerMain where
import CompilerTypes (ConstantPool, Bytecode, Ast)
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

concatTuples :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatTuples (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)

compileDeclarations :: Ast -> (ConstantPool, Bytecode)
compileDeclarations [] = ([], [])
compileDeclarations ((Function fun):ast) =
    compileFunction fun `concatTuples` compileDeclarations ast
compileDeclarations ((Struct struct):ast) =
    compileStruct struct `concatTuples` compileDeclarations ast
compileDeclarations ((Enum enum):ast) =
    compileEnum enum `concatTuples` compileDeclarations ast
compileDeclarations ((Typedef typedef):ast) =
    compileTypedef typedef `concatTuples` compileDeclarations ast

compilerMain :: Ast -> (ConstantPool, Bytecode)
compilerMain ast = compileDeclarations (ast)