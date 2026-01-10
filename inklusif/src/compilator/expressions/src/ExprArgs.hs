{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module ExprArgs (compileArgs) where

import Ast (Expr)
import CompilerTypes (CompilerData)

compileArgs
  :: (Expr -> CompilerData -> Either String CompilerData)
  -> [Expr]
  -> CompilerData
  -> Either String CompilerData
compileArgs _ [] prog = Right prog
compileArgs rec (e:es) prog =
  case rec e prog of
    Left err -> Left err
    Right p1 -> compileArgs rec es p1
