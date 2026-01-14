module FieldAccess (compileFieldAccess) where

import CompilerTypes (CompilerData)
import Ast (FieldAccessExpr)

compileFieldAccess :: FieldAccessExpr -> CompilerData -> Either String CompilerData
compileFieldAccess _ prog = Right prog