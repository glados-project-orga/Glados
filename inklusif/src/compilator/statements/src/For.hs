module For (compileFor) where
import CompilerTypes (CompilerData)
import Ast (ForStmt)

compileFor :: ForStmt -> CompilerData -> Either String CompilerData
compileFor _ prog = Right prog