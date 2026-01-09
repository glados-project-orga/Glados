module ArrayIndex (compileArrayIndex) where
import CompilerTypes (CompilerData)
import Ast (ArrayIndexExpr)

compileArrayIndex :: ArrayIndexExpr -> CompilerData -> Either String CompilerData
compileArrayIndex _ prog = Right prog