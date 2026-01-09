module While (compileWhile) where
import CompilerTypes (CompilerData)
import Ast (WhileStmt)

compileWhile :: WhileStmt -> CompilerData -> Either String CompilerData
compileWhile _ prog = Right prog