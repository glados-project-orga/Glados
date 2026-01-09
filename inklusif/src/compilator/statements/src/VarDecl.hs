module VarDecl (compileVarDecl) where
import CompilerTypes (CompilerData)
import Ast (VarDecl)

compileVarDecl :: VarDecl -> CompilerData -> Either String CompilerData
compileVarDecl _ prog = Right prog