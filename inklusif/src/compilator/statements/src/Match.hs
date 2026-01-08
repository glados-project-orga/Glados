module Match (compileMatch) where
import CompilerTypes (ProgramBinary)
import Ast (MatchStmt)

compileMatch :: MatchStmt -> ProgramBinary -> ProgramBinary
compileMatch _ prog = prog