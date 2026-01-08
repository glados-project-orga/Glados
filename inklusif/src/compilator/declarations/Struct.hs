module Struct (compileStruct) where
import Ast (StructDecl(..))
import CompilerTypes (ProgramBinary)

compileStruct :: StructDecl -> ProgramBinary -> ProgramBinary
compileStruct _ prog = prog