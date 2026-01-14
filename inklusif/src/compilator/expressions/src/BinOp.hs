{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module BinOp (compileBinOpExpr) where

import Ast (Expr, BinOp(..))
import CompilerTypes (CompilerData, CompileExpr, CompileResult)
import CompilerTools (appendBody)
import EitherUtils (bindE)

appendBC :: [String] -> CompilerData -> CompilerData
appendBC bc prog = appendBody prog bc

compileBinOpExpr :: CompileExpr -> BinOp -> Expr -> Expr -> CompilerData -> CompileResult
compileBinOpExpr rec Add a b prog = bin rec ["iadd"] a b prog
compileBinOpExpr rec Sub a b prog = bin rec ["isub"] a b prog
compileBinOpExpr rec Mul a b prog = bin rec ["imul"] a b prog
compileBinOpExpr rec Div a b prog = bin rec ["idiv"] a b prog
compileBinOpExpr rec Mod a b prog = bin rec ["irem"] a b prog
compileBinOpExpr _ op _ _ _ = Left ("BinOp not implemented yet: " ++ show op)

bin :: CompileExpr -> [String] -> Expr -> Expr -> CompilerData -> CompileResult
bin rec opBC a b prog =
  bindE (rec a prog) (\p1 ->
    bindE (rec b p1) (\p2 ->
      Right (appendBC opBC p2)))
