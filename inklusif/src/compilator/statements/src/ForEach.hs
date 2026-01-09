module ForEach (compileForEach) where
import CompilerTypes (ProgramLayer)
import Ast (ForEachStmt)

compileForEach :: ForEachStmt -> ProgramLayer -> Either String ProgramLayer
compileForEach _ prog = Right prog