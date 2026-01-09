module Throw (compileThrow) where
import CompilerTypes (ProgramLayer)
import Ast (ThrowStmt)

compileThrow :: ThrowStmt -> ProgramLayer -> Either String ProgramLayer
compileThrow _ prog = Right prog