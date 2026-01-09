{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expre
-}

module Expr (compileExpr) where

import Ast
  ( Expr(..)
  , Literal(..)
  , BinOp(..)
  , CallExpr(..)
  , FunctionDecl(..)
  )
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)

appendBC :: CompilerData -> [String] ->CompilerData
appendBC prog bc = appendBody prog bc

isFunctionDefined :: String -> CompilerData -> Bool
isFunctionDefined name (_, (funs, _, _, _),_ ,_) =
  any (\f -> funcName f == name) funs

emitPushInt :: Int -> [String]
emitPushInt n
  | n == (-1) = ["iconst_m1"]
  | n == 0    = ["iconst_0"]
  | n == 1    = ["iconst_1"]
  | n == 2    = ["iconst_2"]
  | n == 3    = ["iconst_3"]
  | n == 4    = ["iconst_4"]
  | n == 5    = ["iconst_5"]
  | n >= (-128) && n <= 127 = ["bipush " ++ show n]
  | n >= (-32768) && n <= 32767 = ["sipush " ++ show n]
  | otherwise = ["ldc " ++ show n]

emitBinOp :: BinOp -> Either String [String]
emitBinOp op =
  case op of
    Add -> Right ["iadd"]
    Sub -> Right ["isub"]
    Mul -> Right ["imul"]
    Div -> Right ["idiv"]
    Mod -> Right ["irem"]
    _   -> Left ("BinOp not implemented yet: " ++ show op)

emitCall :: String -> [String]
emitCall fname = ["invokestatic " ++ fname] --  à relier avc vm

compileExpr :: Expr -> CompilerData -> Either String CompilerData
compileExpr expr layer =
  case expr of
    LitExpr (IntLit n) ->
       (Right (appendBC layer (emitPushInt n)))

    VarExpr name ->
      Left ("VarExpr not handled yet: " ++ name) -- slot iload

    BinOpExpr op a b ->
      case compileExpr a layer of
        Left err -> Left err
        Right prog1 ->
          case compileExpr b prog1 of
            Left err -> Left err
            Right prog2 ->
              case emitBinOp op of
                Left err -> Left err
                Right bc ->
                  Right (appendBC prog2 bc)

    CallExpression (CallExpr fname args) ->
      if not (isFunctionDefined fname layer)
        then Left ("Undefined function: " ++ fname)
        else
            case compileArgs args layer of
              Left err -> Left err
              Right n_layer ->
                 Right (appendBC n_layer (emitCall fname))

    _ ->
      Left "expression not implemented yet"

compileArgs :: [Expr] -> CompilerData -> Either String CompilerData
compileArgs args layer =
  case args of
    [] -> Right layer
    e : rest ->
      case compileExpr e layer of
        Left err -> Left err
        Right layer' ->
          compileArgs rest layer'
