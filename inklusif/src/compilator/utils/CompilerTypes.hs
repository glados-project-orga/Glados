module CompilerTypes (
    ConstantPool,
    Bytecode,
    SymbolTable,
    Ast,
    ProgramBinary,
    ProgramLayer,
    Defines
) where

import Ast (
    Declaration(..),
    Literal(..),
    FunctionDecl,
    StructDecl,
    EnumDecl,
    TypedefDecl
    )

type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = ([FunctionDecl], [StructDecl], [EnumDecl], [TypedefDecl])
type SymbolTable = [(String, Literal)]
type ProgramBinary = (ConstantPool, Defines, Bytecode)
type ProgramLayer = (ProgramBinary, SymbolTable)