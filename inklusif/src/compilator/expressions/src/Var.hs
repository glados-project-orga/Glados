{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Var (compileVarExpr) where

import CompilerTypes (CompilerData)
import SymbolTableUtils (getVarIndex, getVarType)
import CompilerTools (appendBody, typePrefixVal)

compileVarExpr :: String -> CompilerData -> Either String CompilerData
compileVarExpr varName prog = 
    getVarIndex varName prog >>= \varIndex ->
    getVarType varName prog >>= \varVal ->
    Right (appendBody prog [typePrefixVal varVal ++ "load " ++ show varIndex])
