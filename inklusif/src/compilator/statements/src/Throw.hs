module Throw (compileThrow) where
import CompilerTypes (ProgramLayer)
import Ast (ThrowStmt)

compileThrow :: ThrowStmt -> ProgramLayer -> ProgramLayer
compileThrow _ prog = prog