{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module BinOp (compileBinOpExpr) where

import Ast (Expr, BinOp(..))
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import EitherUtils (bindE)

appendBC :: [String] -> CompilerData -> CompilerData
appendBC bc prog = appendBody prog bc

compileBinOpExpr :: (Expr -> CompilerData -> Either String CompilerData)
  -> BinOp
  -> Expr
  -> Expr
  -> CompilerData
  -> Either String CompilerData
compileBinOpExpr rec Add a b prog = binT rec ["iadd"] a b prog
compileBinOpExpr rec Sub a b prog = binT rec ["isub"] a b prog
compileBinOpExpr rec Mul a b prog = binT rec ["imul"] a b prog
compileBinOpExpr rec Div a b prog = binT rec ["idiv"] a b prog
compileBinOpExpr rec Mod a b prog = binT rec ["irem"] a b prog
compileBinOpExpr _ op _ _ _ = Left ("Typed BinOp not implemented yet: " ++ show op)

binT :: (Expr -> CompilerData -> Either String CompilerData)
  -> [String]
  -> Expr
  -> Expr
  -> CompilerData
  -> Either String CompilerData
binT rec opBC a b prog =
  bindE (rec a prog) (\p1 ->
    bindE (rec b p1) (\p2 ->
      Right (appendBC opBC p2)))