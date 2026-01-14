{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module BinOp (compileBinOpExprT) where

import Ast (Expr, BinOp(..), Type(..))
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import EitherUtils (bindE, thenE)

appendBC :: [String] -> CompilerData -> CompilerData
appendBC bc prog = appendBody prog bc

compileBinOpExprT
  :: (Expr -> CompilerData -> Either String (Type, CompilerData))
  -> BinOp
  -> Expr
  -> Expr
  -> CompilerData
  -> Either String (Type, CompilerData)
compileBinOpExprT rec Add a b prog = binT rec ["iadd"] a b prog
compileBinOpExprT rec Sub a b prog = binT rec ["isub"] a b prog
compileBinOpExprT rec Mul a b prog = binT rec ["imul"] a b prog
compileBinOpExprT rec Div a b prog = binT rec ["idiv"] a b prog
compileBinOpExprT rec Mod a b prog = binT rec ["irem"] a b prog
compileBinOpExprT _ op _ _ _ = Left ("Typed BinOp not implemented yet: " ++ show op)

binT
  :: (Expr -> CompilerData -> Either String (Type, CompilerData))
  -> [String]
  -> Expr
  -> Expr
  -> CompilerData
  -> Either String (Type, CompilerData)
binT rec opBC a b prog =
  bindE (rec a prog) (\(ta, p1) ->
    bindE (rec b p1) (\(tb, p2) ->
      thenE (expectInt2 ta tb)
        (Right (IntType, appendBC opBC p2))))

isIntLike :: Type -> Bool
isIntLike IntType = True
isIntLike _ = False

expectInt2 :: Type -> Type -> Either String ()
expectInt2 ta tb
  | isIntLike ta && isIntLike tb = Right ()
  | otherwise =
      Left ("type error: expected (int, int) but got (" ++ show ta ++ ", " ++ show tb ++ ")")
