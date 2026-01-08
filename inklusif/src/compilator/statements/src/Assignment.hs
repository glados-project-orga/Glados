module Assignment (compileAssignment) where
import CompilerTypes (ProgramLayer)
import Ast (Assignment)

compileAssignment :: Assignment -> ProgramLayer -> ProgramLayer
compileAssignment _ prog = prog