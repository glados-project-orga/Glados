module Typedef (compileTypedef) where
import Ast (TypedefDecl(..))
import CompilerTypes (ConstantPool, Bytecode)

compileTypedef :: TypedefDecl -> (ConstantPool, Bytecode)
compileTypedef _ = ([], [])