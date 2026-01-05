module Struct (compileStruct) where
import Ast (StructDecl(..))
import CompilerTypes (ProgramBinary)

compileStruct :: StructDecl -> ProgramBinary
compileStruct _ = ([], [])