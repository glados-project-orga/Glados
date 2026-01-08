module Assignment (compileAssignment) where
import CompilerTypes (ProgramBinary)
import Ast (Assignment)

compileAssignment :: Assignment -> ProgramBinary -> ProgramBinary
compileAssignment _ prog = prog