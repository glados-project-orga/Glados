module CompilerTypes (
    ConstantPool,
    Bytecode,
    SymbolTable,
    Ast,
    CompilerData,
    Defines
) where

import Ast (
    Declaration(..),
    FunctionDecl,
    StructDecl,
    EnumDecl,
    TypedefDecl
    )

type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = ([FunctionDecl], [StructDecl], [EnumDecl], [TypedefDecl])
type SymbolTable = [(String, Int)]
type CompilerData = (ConstantPool, Defines, Bytecode, SymbolTable)