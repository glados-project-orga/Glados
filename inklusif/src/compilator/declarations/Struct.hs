module Struct (compileStruct) where
import Ast (StructDecl(..))
import CompilerTypes (ProgramBinary)

compileStruct :: StructDecl -> ProgramBinary -> Either String ProgramBinary
compileStruct _ prog = Right prog