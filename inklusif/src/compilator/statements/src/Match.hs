module Match (compileMatch) where
import CompilerTypes (CompilerData)
import Ast (MatchStmt)

compileMatch :: MatchStmt -> CompilerData -> Either String CompilerData
compileMatch _ prog = Right prog