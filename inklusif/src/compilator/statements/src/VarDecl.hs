module VarDecl (compileVarDecl) where
import CompilerTypes (ProgramLayer)
import Ast (VarDecl)

compileVarDecl :: VarDecl -> ProgramLayer -> ProgramLayer
compileVarDecl _ prog = prog