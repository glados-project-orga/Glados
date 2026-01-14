{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Lookup (getFunctions, findFunction) where

import Ast (FunctionDecl(..))
import CompilerTypes (Defines)

getFunctions :: Defines -> [FunctionDecl]
getFunctions (_, funs, _, _, _) = funs

findFunction :: String -> [FunctionDecl] -> Maybe FunctionDecl
findFunction _ [] = Nothing
findFunction name (f:fs)
  | funcName f == name = Just f
  | otherwise          = findFunction name fs
