{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Literal (compileLiteralExpr) where

import Ast (Literal(..))
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)

compileLiteralExpr :: Literal -> CompilerData -> Either String CompilerData
compileLiteralExpr (IntLit n) prog = Right (appendBody prog (emitPushInt n))
compileLiteralExpr _ _ = Left "Literal not implemented yet"

emitPushInt :: Int -> [String]
emitPushInt (-1) = ["iconst_m1"]
emitPushInt 0    = ["iconst_0"]
emitPushInt 1    = ["iconst_1"]
emitPushInt 2    = ["iconst_2"]
emitPushInt 3    = ["iconst_3"]
emitPushInt 4    = ["iconst_4"]
emitPushInt 5    = ["iconst_5"]
emitPushInt n
  | n >= (-128) && n <= 127       = ["bipush " ++ show n]
  | n >= (-32768) && n <= 32767   = ["sipush " ++ show n]
  | otherwise                     = ["ldc " ++ show n]
