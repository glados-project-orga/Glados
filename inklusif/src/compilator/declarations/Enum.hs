module Enum (compileEnum) where
import Ast (EnumDecl(..))
import CompilerTypes (ProgramBinary)

compileEnum :: EnumDecl -> ProgramBinary -> Either String ProgramBinary
compileEnum _ prog = Right prog