module Enum (compileEnum) where
import Ast (EnumDecl(..))
import CompilerTypes (ProgramBinary)

compileEnum :: EnumDecl -> ProgramBinary -> ProgramBinary
compileEnum _ prog = prog