module Throw (compileThrow) where
import CompilerTypes (ProgramBinary)
import Ast (ThrowStmt)

compileThrow :: ThrowStmt -> ProgramBinary -> ProgramBinary
compileThrow _ prog = prog