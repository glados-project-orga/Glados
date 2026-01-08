module CompilerTools (
    appendProgramBinaries,
    appendToHeader,
    appendToDefines,
    appendToBody) where
import CompilerTypes(ProgramBinary, ConstantPool, Defines, Bytecode)

appendProgramBinaries :: ProgramBinary -> ProgramBinary -> ProgramBinary
appendProgramBinaries (head1, def1, body1) (head2, def2, body2) =
    (head1 ++ head2, def1 ++ def2, body1 ++ body2)

appendToHeader :: ProgramBinary -> ConstantPool -> ProgramBinary
appendToHeader (header, def, body) newHead = (header ++ newHead, def, body)

appendToDefines :: ProgramBinary -> Defines -> ProgramBinary
appendToDefines (header, def, body) newDef = (header, def ++ newDef, body)

appendToBody :: ProgramBinary -> Bytecode -> ProgramBinary
appendToBody (header, def, body) newBody = (header, def, body ++ newBody)