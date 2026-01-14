{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- Call.hs
-}

module Call (compileCallExpr) where

import CompilerTypes (CompilerData)

compileCallExpr:: String -> CompilerData -> Either String CompilerData
compileCallExpr _ prog = Right prog