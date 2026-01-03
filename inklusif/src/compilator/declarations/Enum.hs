module Enum (compileEnum) where
import Ast (EnumDecl(..))
import CompilerTypes (ConstantPool, Bytecode)

compileEnum :: EnumDecl -> (ConstantPool, Bytecode)
compileEnum _ = ([], [])