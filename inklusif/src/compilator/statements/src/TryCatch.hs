module TryCatch (compileTryCatch) where
import CompilerTypes (ProgramLayer)
import Ast (TryCatchStmt)

compileTryCatch :: TryCatchStmt -> ProgramLayer -> ProgramLayer
compileTryCatch _ prog = prog