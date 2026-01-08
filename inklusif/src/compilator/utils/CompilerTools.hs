module CompilerTools (
    appendProgramBinaries,
    appendHeader,
    appendDefines,
    appendBody) where
import CompilerTypes(ProgramBinary, ConstantPool, Defines, Bytecode)

appendProgramBinaries :: ProgramBinary -> ProgramBinary -> ProgramBinary
appendProgramBinaries (head1, def1, body1) (head2, def2, body2) =
    (head1 ++ head2, def1 ++ def2, body1 ++ body2)

appendHeader :: ProgramBinary -> ConstantPool -> ProgramBinary
appendHeader (header, def, body) newHead = (header ++ newHead, def, body)

appendDefines :: ProgramBinary -> Defines -> ProgramBinary
appendDefines (header, def, body) newDef = (header, def ++ newDef, body)

appendBody :: ProgramBinary -> Bytecode -> ProgramBinary
appendBody (header, def, body) newBody = (header, def, body ++ newBody)