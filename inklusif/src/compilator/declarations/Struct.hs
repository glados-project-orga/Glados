module Struct (compileStruct) where
import Ast (StructDecl(..))
import CompilerTypes (ConstantPool, Bytecode)

compileStruct :: StructDecl -> (ConstantPool, Bytecode)
compileStruct _ = ([], [])