import AST
module CompilerTypes (
    ConstantPool,
    Bytecode,
    SymbolTable,
) where

type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type SymbolTable = [(String, Int)]