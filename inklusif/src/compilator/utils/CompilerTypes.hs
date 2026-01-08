module CompilerTypes (
    ConstantPool,
    Bytecode,
    SymbolTable,
    Ast,
    ProgramBinary
) where

import Ast (Declaration, Declaration(..))

type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = [Declaration]
type SymbolTable = [(String, Int)]
type ProgramBinary = (ConstantPool, Defines, Bytecode)