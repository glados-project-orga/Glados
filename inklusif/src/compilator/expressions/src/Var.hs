{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Var (compileVarExpr) where

import CompilerTypes (CompilerData)
import SymbolTableUtils (getVarIndex, getVarType)
import CompilerTools (appendBody, typePrefixVal, getEnumValue)

compileVarExpr :: String -> CompilerData -> Either String CompilerData
compileVarExpr varName prog@(_, (_, _, _, enums, _, _), _, _)
    = case getEnumValue enums varName of
        Just enumVal -> Right (appendBody prog ["iconst " ++ show enumVal])
        _ -> getVarIndex varName prog >>= \varIndex ->
             getVarType varName prog >>= \varVal ->
             Right (appendBody prog [typePrefixVal varVal ++ "load " ++ show varIndex])