module StructLiteral (compileStructLiteral) where

import CompilerTypes (CompilerData)
import Ast (Expr)

compileStructLiteral :: String -> [(String, Expr)] -> CompilerData -> Either String CompilerData
compileStructLiteral _ _ prog = Right prog