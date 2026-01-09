module While (compileWhile) where
import CompilerTypes (ProgramLayer)
import Ast (WhileStmt)

compileWhile :: WhileStmt -> ProgramLayer -> Either String ProgramLayer
compileWhile _ prog = Right prog