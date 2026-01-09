module Typedef (compileTypedef) where
import Ast (TypedefDecl(..))
import CompilerTypes (CompilerData)

compileTypedef :: TypedefDecl  -> CompilerData -> Either String CompilerData
compileTypedef _ prog = Right prog