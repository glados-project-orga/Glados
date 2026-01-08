module While (compileWhile) where
import CompilerTypes (ProgramLayer)
import Ast (WhileStmt)

compileWhile :: WhileStmt -> ProgramLayer -> ProgramLayer
compileWhile _ prog = prog