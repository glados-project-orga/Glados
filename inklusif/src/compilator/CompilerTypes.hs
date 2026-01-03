module CompilerTypes (
    ConstantPool,
    Bytecode,
    SymbolTable,
    Ast
) where

import Ast (Declaration)

type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type SymbolTable = [(String, Int)]