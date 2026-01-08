module Match (compileMatch) where
import CompilerTypes (ProgramLayer)
import Ast (MatchStmt)

compileMatch :: MatchStmt -> ProgramLayer -> ProgramLayer
compileMatch _ prog = prog