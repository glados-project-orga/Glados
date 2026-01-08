module CompilerTypes (
    ConstantPool,
    Bytecode,
    SymbolTable,
    Ast,
    ProgramBinary,
    ProgramLayer,
    Defines
) where

import Ast (Declaration(..), Literal(..))

type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = [Declaration]
type SymbolTable = [(String, Literal)]
type ProgramBinary = (ConstantPool, Defines, Bytecode)
type ProgramLayer = (ProgramBinary, SymbolTable)