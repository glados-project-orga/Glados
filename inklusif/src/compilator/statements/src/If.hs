module If (compileIf) where
import CompilerTypes (CompilerData)
import Ast (IfStmt)

compileIf :: IfStmt -> CompilerData -> Either String CompilerData
compileIf _ prog = Right prog