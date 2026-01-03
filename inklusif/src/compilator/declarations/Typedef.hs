module Typedef (compileTypedef) where
import ast AST (TypedefDecl(..))

compileTypedef :: TypedefDecl -> (ConstantPool, Bytecode)
compileTypedef _ _ _ = ([], [])