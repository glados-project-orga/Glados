module If (compileIf) where
import CompilerTypes (ProgramBinary)
import Ast (IfStmt)

compileIf :: IfStmt -> ProgramBinary -> ProgramBinary
compileIf _ prog = prog