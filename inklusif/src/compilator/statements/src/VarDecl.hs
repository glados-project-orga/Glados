module VarDecl (compileVarDecl) where
import CompilerTypes (ProgramLayer)
import Ast (VarDecl)

compileVarDecl :: VarDecl -> ProgramLayer -> Either String ProgramLayer
compileVarDecl _ prog = Right prog