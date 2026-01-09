module TryCatch (compileTryCatch) where
import CompilerTypes (ProgramLayer)
import Ast (TryCatchStmt)

compileTryCatch :: TryCatchStmt -> ProgramLayer -> Either String ProgramLayer
compileTryCatch _ prog = Right prog