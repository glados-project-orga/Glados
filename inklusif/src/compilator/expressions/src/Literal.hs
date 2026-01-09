module Literal (compileLiteral) where
import CompilerTypes (CompilerData)
import Ast (Literal)

compileLiteral :: Literal -> CompilerData -> Either String CompilerData
compileLiteral _ prog = Right prog