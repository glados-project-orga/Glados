{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Var (compileVarExpr) where

import CompilerTypes (CompilerData)

compileVarExpr :: String -> CompilerData -> Either String CompilerData
compileVarExpr _ prog = Right prog
