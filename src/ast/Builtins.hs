{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- Builtins.hs
-}

module Builtins (
    apply
) where

import Data(Ast(..))

apply :: String -> [Ast] -> Maybe Ast
apply "+" [AInt a, AInt b] = Just (AInt (a + b))
apply "-" [AInt a, AInt b] = Just (AInt (a - b))
apply "*" [AInt a, AInt b] = Just (AInt (a * b))
apply _ _ = Nothing