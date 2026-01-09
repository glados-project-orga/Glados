module TryCatch (compileTryCatch) where
import CompilerTypes (CompilerData)
import Ast (TryCatchStmt)

compileTryCatch :: TryCatchStmt -> CompilerData -> Either String CompilerData
compileTryCatch _ prog = Right prog