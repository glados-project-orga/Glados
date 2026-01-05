module CompilerTypes (
    ConstantPool,
    Bytecode,
    SymbolTable,
    Ast,
    ProgramBinary
) where

import Ast (Declaration)

type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type SymbolTable = [(String, Int)]
type ProgramBinary = (ConstantPool, Bytecode)