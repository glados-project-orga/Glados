module Assignment (compileAssignment) where
import CompilerTypes (ProgramLayer)
import Ast (Assignment)

compileAssignment :: Assignment -> ProgramLayer -> Either String ProgramLayer
compileAssignment _ prog = Right prog