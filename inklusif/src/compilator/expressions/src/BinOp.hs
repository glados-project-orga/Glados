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

compileBinOpExpr
  :: (Expr -> CompilerData -> Either String CompilerData)
  -> BinOp
  -> Expr
  -> Expr
  -> CompilerData
  -> Either String CompilerData
compileBinOpExpr rec Add a b prog = bin rec ["iadd"] a b prog
compileBinOpExpr rec Sub a b prog = bin rec ["isub"] a b prog
compileBinOpExpr rec Mul a b prog = bin rec ["imul"] a b prog
compileBinOpExpr rec Div a b prog = bin rec ["idiv"] a b prog
compileBinOpExpr rec Mod a b prog = bin rec ["irem"] a b prog
compileBinOpExpr _ op _ _ _ = Left ("BinOp not implemented yet: " ++ show op)

bin
  :: (Expr -> CompilerData -> Either String CompilerData)
  -> [String]
  -> Expr
  -> Expr
  -> CompilerData
  -> Either String CompilerData
bin rec opBC a b prog =
  case rec a prog of
    Left err -> Left err
    Right p1 ->
      case rec b p1 of
        Left err -> Left err
        Right p2 -> Right (appendBC opBC p2)

appendBC :: [String] -> CompilerData -> CompilerData
appendBC bc prog = appendBody prog bc
