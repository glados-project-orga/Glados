{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Var (compileVarExpr) where

import CompilerTypes (CompilerData, SymInfo(..))
import SymbolTableUtils (getVar)
import CompilerTools (appendBody, typePrefixVal, getEnumValue)

compileVarExpr :: String -> CompilerData -> Either String CompilerData
compileVarExpr varName prog@(_, (_, _, _, enums, _, _), _, _)
    = case getEnumValue enums varName of
        Just enumVal -> Right (appendBody prog ["iconst " ++ show enumVal])
        _ -> getVar varName prog >>= \varVal -> case varVal of
                (SymInfo _ _ (Just cindex) _) -> Right (appendBody prog ["ldc " ++ show cindex])
                (SymInfo index t _ _)-> Right (appendBody prog [typePrefixVal t ++ "load " ++ show index])