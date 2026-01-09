module Struct (compileStruct) where
import Ast (StructDecl(..))
import CompilerTypes (CompilerData)

compileStruct :: StructDecl -> CompilerData -> Either String CompilerData
compileStruct _ prog = Right prog