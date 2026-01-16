module MethodCall (compileMethodCall) where

import CompilerTypes (CompilerData)
import Ast (MethodCallExpr(..), Expr(..))
import CompilerTools (appendBody)
import Expr (compileExpr)

pushArgs :: [Expr] -> CompilerData -> Either String CompilerData
pushArgs [] prog = Right prog
pushArgs (e:es) prog = pushArgs es prog >>= \n_prog -> compileExpr e n_prog

compileMethodCall :: MethodCallExpr -> CompilerData -> Either String CompilerData
compileMethodCall (MethodCallExpr obj name exprs) prog = pushArgs (obj:exprs) prog >>= \n_prog ->
    Right (appendBody n_prog ["invokstatic " ++ name])