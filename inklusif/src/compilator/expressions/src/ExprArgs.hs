{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr args
-}

module ExprArgs (compileArgs, compileArgsT) where

import Ast (Expr, Type)
import CompilerTypes (CompilerData)

bindE :: Either String a -> (a -> Either String b) -> Either String b
bindE (Left err) _ = Left err
bindE (Right x) f  = f x

compileArgs
  :: (Expr -> CompilerData -> Either String CompilerData)
  -> [Expr]
  -> CompilerData
  -> Either String CompilerData
compileArgs _ [] prog = Right prog
compileArgs rec (e:es) prog =
  bindE (rec e prog) (\p1 -> compileArgs rec es p1)

compileArgsT
  :: (Expr -> CompilerData -> Either String (Type, CompilerData))
  -> [Expr]
  -> CompilerData
  -> Either String ([Type], CompilerData)
compileArgsT _ [] prog = Right ([], prog)
compileArgsT rec (e:es) prog =
  bindE (rec e prog) (\(t, p1) ->
    bindE (compileArgsT rec es p1) (\(ts, p2) ->
      Right (t:ts, p2)))
