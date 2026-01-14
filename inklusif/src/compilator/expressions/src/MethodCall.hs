module MethodCall (compileMethodCall) where

import CompilerTypes (CompilerData)
import Ast (MethodCallExpr)

compileMethodCall :: MethodCallExpr -> CompilerData -> Either String CompilerData
compileMethodCall _ prog = Right prog