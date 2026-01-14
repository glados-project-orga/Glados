module ArrayLiteral (compileArrayLiteral) where

import CompilerTypes (CompilerData)
import Ast (Expr)

compileArrayLiteral :: [Expr] -> CompilerData -> Either String CompilerData
compileArrayLiteral _ prog = Right prog
