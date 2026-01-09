module Match (compileMatch) where
import CompilerTypes (ProgramLayer)
import Ast (MatchStmt)

compileMatch :: MatchStmt -> ProgramLayer -> Either String ProgramLayer
compileMatch _ prog = Right prog