module Enum (compileEnum) where
import ast AST (EnumDecl(..))

compileEnum :: EnumDecl -> (ConstantPool, Bytecode)
compileEnum _ _ _ = ([], [])