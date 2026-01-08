module For (compileFor) where
import CompilerTypes (ProgramLayer)
import Ast (ForStmt)

compileFor :: ForStmt -> ProgramLayer -> ProgramLayer
compileFor _ prog = prog