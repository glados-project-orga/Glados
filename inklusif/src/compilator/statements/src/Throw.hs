module Throw (compileThrow) where
import CompilerTypes (CompilerData)
import Ast (ThrowStmt)

compileThrow :: ThrowStmt -> CompilerData -> Either String CompilerData
compileThrow _ prog = Right prog