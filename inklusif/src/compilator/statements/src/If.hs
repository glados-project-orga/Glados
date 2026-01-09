module If (compileIf) where
import CompilerTypes (ProgramLayer)
import Ast (IfStmt)

compileIf :: IfStmt -> ProgramLayer -> Either String ProgramLayer
compileIf _ prog = Right prog