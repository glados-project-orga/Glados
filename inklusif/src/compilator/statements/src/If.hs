module If (compileIf) where
import CompilerTypes (ProgramLayer)
import Ast (IfStmt)

compileIf :: IfStmt -> ProgramLayer -> ProgramLayer
compileIf _ prog = prog