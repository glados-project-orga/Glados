module ForEach (compileForEach) where
import CompilerTypes (ProgramLayer)
import Ast (ForEachStmt)

compileForEach :: ForEachStmt -> ProgramLayer -> ProgramLayer
compileForEach _ prog = prog