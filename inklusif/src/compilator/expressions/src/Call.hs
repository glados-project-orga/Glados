{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- Call.hs
-}

module Call (compileCallExpr) where

import CompilerTypes (CompilerData)
import FunctionUtils (searchFunctions)
import CompilerTools (appendBody)
import Ast (Expr(..), CallExpr(..))
import Expr (compileExpr)

pushArgs :: [Expr] -> CompilerData -> Either String CompilerData
pushArgs [] prog = Right prog
pushArgs (e:es) prog = pushArgs es prog >>= \n_prog -> compileExpr e n_prog

compileCallExpr:: CallExpr -> CompilerData -> Either String CompilerData
compileCallExpr (CallExpr name exprs) prog = pushArgs exprs prog >>= \n_prog ->
    Right (appendBody n_prog ["invokstatic " ++ name])