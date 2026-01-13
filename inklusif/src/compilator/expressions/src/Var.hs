{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Var (compileVarExpr, compileVarExprT) where

import Ast (Type)
import CompilerTypes (CompilerData, SymbolTable, SymInfo(..))
import CompilerTools (appendBody)
import EitherUtils (bindE)

compileVarExpr :: String -> CompilerData -> Either String CompilerData
compileVarExpr name prog = snd <$> compileVarExprT name prog

compileVarExprT :: String -> CompilerData -> Either String (Type, CompilerData)
compileVarExprT name prog@(_, _, _, st) =
  bindE (emitLoadVarT name st) (\(t, bc) ->
    Right (t, appendBody prog bc))

emitLoadVarT :: String -> SymbolTable -> Either String (Type, [String])
emitLoadVarT name st =
  bindE (lookupVar name st) (\info ->
    Right (symType info, [emitLoadIdx (symIndex info)]))

lookupVar :: String -> SymbolTable -> Either String SymInfo
lookupVar name st =
  maybeToEither ("Undefined variable: " ++ name) (lookup name st)

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err Nothing  = Left err
maybeToEither _   (Just x) = Right x

emitLoadIdx :: Int -> String
emitLoadIdx 0 = "iload_0"
emitLoadIdx 1 = "iload_1"
emitLoadIdx 2 = "iload_2"
emitLoadIdx 3 = "iload_3"
emitLoadIdx n = "iload " ++ show n
