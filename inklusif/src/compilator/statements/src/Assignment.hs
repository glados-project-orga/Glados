module Assignment (compileAssignment) where
import CompilerTypes (CompilerData)
import Ast (Assignment)

compileAssignment :: Assignment -> CompilerData -> Either String CompilerData
compileAssignment _ prog = Right prog