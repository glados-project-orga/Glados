module ForEach (compileForEach) where
import CompilerTypes (ProgramBinary)
import Ast (ForEachStmt)

compileForEach :: ForEachStmt -> ProgramBinary -> ProgramBinary
compileForEach _ prog = prog