{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Literal (compileLiteralExpr, compileLiteralExprT) where

import Ast (Literal(..), Type(..))
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)

compileLiteralExpr :: Literal -> CompilerData -> Either String CompilerData
compileLiteralExpr lit prog = snd <$> compileLiteralExprT lit prog

compileLiteralExprT :: Literal -> CompilerData -> Either String (Type, CompilerData)
compileLiteralExprT (IntLit n) prog = Right (IntType, appendBody prog (emitPushInt n))
compileLiteralExprT (FloatLit x) prog = Right (FloatType, appendBody prog (emitPushFloat x))
compileLiteralExprT (BoolLit True) prog = Right (BoolType, appendBody prog ["iconst_1"])
compileLiteralExprT (BoolLit False) prog = Right (BoolType, appendBody prog ["iconst_0"])
compileLiteralExprT (CharLit c) prog = Right (CharType, appendBody prog (emitPushInt (fromEnum c)))
compileLiteralExprT (StringLit s) prog = Right (StringType, appendBody prog ["ldc \"" ++ s ++ "\""])
compileLiteralExprT _ _ = Left "Literal not implemented yet"

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

emitPushFloat :: Double -> [String]
emitPushFloat 0.0 = ["fconst_0"]
emitPushFloat 1.0 = ["fconst_1"]
emitPushFloat 2.0 = ["fconst_2"]
emitPushFloat x   = ["ldc " ++ show x]
