module CompilerTools (appendProgramBinaries) where
import CompilerTypes(ProgramBinary)

appendProgramBinaries :: ProgramBinary -> ProgramBinary -> ProgramBinary
appendProgramBinaries (head1, def1, body1) (head2, def2, body2) =
    (head1 ++ head2, def1 ++ def2, body1 ++ body2)