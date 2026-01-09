module Enum (compileEnum) where
import Ast (EnumDecl(..))
import CompilerTypes (CompilerData)

compileEnum :: EnumDecl -> CompilerData -> Either String CompilerData
compileEnum _ prog = Right prog