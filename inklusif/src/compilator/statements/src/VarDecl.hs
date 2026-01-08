module VarDecl (compileVarDecl) where
import CompilerTypes (ProgramBinary)
import Ast (VarDecl)

compileVarDecl :: VarDecl -> ProgramBinary -> ProgramBinary
compileVarDecl _ prog = prog