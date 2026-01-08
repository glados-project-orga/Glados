module For (compileFor) where
import CompilerTypes (ProgramBinary)
import Ast (ForStmt)

compileFor :: ForStmt -> ProgramBinary -> ProgramBinary
compileFor _ prog = prog