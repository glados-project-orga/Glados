{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- Builtins.hs
-}

-- module Main (main) where
module Builtins (
    apply
) where

import Data(Ast(..))
import qualified Data.Map as Map

apply :: Map.Map String ([Ast] -> Either String Ast)
apply = Map.fromList
  [ ("+", builtinAdd),
    ("*", builtinMul),
    ("-", builtinSub),
    ("mod", builtinMod),
    ("div", builtinDiv)
  ]

foundInt :: Ast -> Either String Int
foundInt (AInt n) = Right n
foundInt _ = Left "Expected interger"

builtinAdd :: [Ast] -> Either String Ast
builtinAdd args =
  case traverse foundInt args of
    Left err -> Left err
    Right vals -> Right (AInt (sum vals))


builtinSub :: [Ast] -> Either String Ast
builtinSub [] = Left "empty"
builtinSub (AInt x: xs) =
  case traverse foundInt xs of
    Left err -> Left err
    Right vals -> Right (AInt (foldl (-) x (vals)))
builtinSub _ = Left "execpted elements"


builtinMul :: [Ast] -> Either String Ast
builtinMul args = 
  case traverse foundInt args of
    Left err -> Left err
    Right vals -> Right (AInt (product vals))


builtinMod :: [Ast] -> Either String Ast
builtinMod args = case args of
  [AInt x, AInt y] ->
    if y == 0 then Left "mod: undefined for 0"
              else Right (AInt(x `mod` y))
  _ -> Left "expected exactly 2 intergers"


builtinDiv :: [Ast] -> Either String Ast
builtinDiv args = case args of
  [AInt x, AInt y] ->
    if y == 0 then Left "division by zero"
              else Right (AInt(x `div` y))
  _ -> Left "expected exactly 2 intergers"

-- main :: IO ()
-- main = do
--   let expr = [AInt 5, AInt 6, AInt 7, AInt 8]
--   let expr1 = [AInt 7, AInt 8]

--   case builtinMod expr1 of
--     Left err -> putStrLn ("error" ++ err)
--     Right ast -> print ast

--   case builtinDiv expr1 of
--     Left err -> putStrLn ("error" ++ err)
--     Right ast -> print ast
  
--   case builtinAdd expr of
--     Left err -> putStrLn ("error" ++ err)
--     Right ast -> print ast
  
--   case builtinSub expr of
--     Left err -> putStrLn ("error" ++ err)
--     Right ast -> print ast
  
--   case builtinMul expr of
--     Left err -> putStrLn ("error" ++ err)
--     Right ast -> print ast
  