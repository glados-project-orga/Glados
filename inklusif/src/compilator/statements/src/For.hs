module For (compileFor) where
import CompilerTypes (ProgramLayer)
import Ast (ForStmt)

compileFor :: ForStmt -> ProgramLayer -> Either String ProgramLayer
compileFor _ prog = Right prog