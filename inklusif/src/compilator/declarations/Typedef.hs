module Typedef (compileTypedef) where
import Ast (TypedefDecl(..))
import CompilerTypes (ProgramBinary)

compileTypedef :: TypedefDecl  -> ProgramBinary -> Either String ProgramBinary
compileTypedef _ prog = Right prog