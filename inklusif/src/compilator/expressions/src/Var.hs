{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Var (compileVarExpr) where

import CompilerTypes (CompilerData, SymbolTable, SymInfo(..))
import CompilerTools (appendBody)

compileVarExpr :: String -> CompilerData -> Either String CompilerData
compileVarExpr name prog@(_, _, _, st) =
  case emitLoadVar name st of
    Left err -> Left err
    Right bc -> Right (appendBody prog bc)

emitLoadVar :: String -> SymbolTable -> Either String [String]
emitLoadVar name st =
  case lookup name st of
    Nothing  -> Left ("Undefined variable: " ++ name)
    Just symInfo -> Right [emitLoadIdx (symIndex symInfo)]

emitLoadIdx :: Int -> String
emitLoadIdx 0 = "iload_0"
emitLoadIdx 1 = "iload_1"
emitLoadIdx 2 = "iload_2"
emitLoadIdx 3 = "iload_3"
emitLoadIdx n = "iload " ++ show n
