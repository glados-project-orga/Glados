module Typedef (compileTypedef) where
import Ast (TypedefDecl(..))
import CompilerTypes (ProgramBinary)

compileTypedef :: TypedefDecl -> ProgramBinary
compileTypedef _ = ([], [])