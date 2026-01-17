{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Arity (checkArity) where

import Ast (Parameter(..), Expr)

checkArity :: String -> [Parameter] -> [Expr] -> Either String ()
checkArity fname params args
  | length params == length args = Right ()
  | otherwise                    = Left ("wrong number of arguments for " ++ fname)
