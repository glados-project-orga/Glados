module TryCatch (compileTryCatch) where
import CompilerTypes (ProgramBinary)
import Ast (TryCatchStmt)

compileTryCatch :: TryCatchStmt -> ProgramBinary -> ProgramBinary
compileTryCatch _ prog = prog