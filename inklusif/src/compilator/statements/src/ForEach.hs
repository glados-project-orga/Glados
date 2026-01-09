module ForEach (compileForEach) where
import CompilerTypes (CompilerData)
import Ast (ForEachStmt)

compileForEach :: ForEachStmt -> CompilerData -> Either String CompilerData
compileForEach _ prog = Right prog