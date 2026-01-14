module ArrayIndex (compileArrayIndex) where

import CompilerTypes (CompilerData)
import Ast (ArrayIndexExpr(..), Expr(..))

compileArrayIndex :: ArrayIndexExpr -> CompilerData -> Either String CompilerData
compileArrayIndex (ArrayIndexExpr arr index val) prog = Right prog