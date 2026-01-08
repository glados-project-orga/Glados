module Typedef (compileTypedef) where
import Ast (TypedefDecl(..))
import CompilerTypes (ProgramBinary)

compileTypedef :: TypedefDecl  -> ProgramBinary -> ProgramBinary
compileTypedef _ prog = prog