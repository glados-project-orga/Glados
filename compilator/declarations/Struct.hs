module Struct (compileStruct) where
import ast AST (StructDecl(..))

compileStruct :: StructDecl -> (ConstantPool, Bytecode)
compileStruct _ _ _ = ([], [])