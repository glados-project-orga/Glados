module While (compileWhile) where
import CompilerTypes (ProgramBinary)
import Ast (WhileStmt)

compileWhile :: WhileStmt -> ProgramBinary -> ProgramBinary
compileWhile _ prog = prog