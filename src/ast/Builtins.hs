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
import qualified Data.Map as Map

-- apply :: String -> [Ast] -> Maybe Ast
-- apply "+" [AInt a, AInt b] = Just (AInt (a + b))
-- apply "-" [AInt a, AInt b] = Just (AInt (a - b))
-- apply "*" [AInt a, AInt b] = Just (AInt (a * b))
-- apply _ _ = Nothing

apply :: Map.Map String ([Ast] -> Either String Ast)
apply = Map.fromList
  [ ("+", builtinAdd),
    ("*", builtinMul),
    ("-", builtinSub),
    ("mod", builtinMod),
    ("div", builtinDiv)
  ]

builtinAdd :: [Ast] -> Either String Ast
builtinAdd _ = Left "Error in operation"


builtinMul :: [Ast] -> Either String Ast
builtinMul _ = Left "Error in operation"

builtinSub :: [Ast] -> Either String Ast
builtinSub _ = Left "Error in operation"

builtinMod :: [Ast] -> Either String Ast
builtinMod _ = Left "Error in operation"

builtinDiv :: [Ast] -> Either String Ast
builtinDiv _ = Left "Error in operation"
