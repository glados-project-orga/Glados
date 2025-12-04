{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- Builtins.hs
-}

-- module Main (main) where
module Builtins (
    apply,
    foundInt,
    builtinAdd,
    builtinSub,
    builtinMul,
    builtinMod,
    builtinDiv,
    builtinEqual,
    builtinLower,
    builtinGreater,
    builtinIf
) where

import Data(Ast(..))
import qualified Data.Map as Map

apply :: Map.Map String ([Ast] -> Either String Ast)
apply = Map.fromList
  [ ("+", builtinAdd),
    ("*", builtinMul),
    ("-", builtinSub),
    ("mod", builtinMod),
    ("div", builtinDiv),
    ("eq?", builtinEqual),
    ("<", builtinLower),
    (">", builtinGreater),
    ("if", builtinIf)
  ]

foundInt :: Ast -> Either String Int
foundInt (AInt n) = Right n
foundInt a = Left (show a ++ " is not a number")

foundBool :: Ast -> Either String Bool
foundBool (ABool b) = Right b
foundBool a = Left (show a ++ " is not a boolean")

builtinAdd :: [Ast] -> Either String Ast
builtinAdd args =
  case traverse foundInt args of
    Left err -> Left ("Exception in +: " ++ err)
    Right vals -> Right (AInt (sum vals))

builtinSub :: [Ast] -> Either String Ast
builtinSub (first:rest) =
  case traverse foundInt (first:rest) of
    Left err -> Left ("Exception in +: " ++ err)
    Right (firstVal:restVal) -> Right (AInt (foldl (-) firstVal restVal))
    Right _ -> Left "Exception: incorrect argument count in call (-)"
builtinSub [] = Left "Exception: incorrect argument count in call (-)"

builtinMul :: [Ast] -> Either String Ast
builtinMul args = 
  case traverse foundInt args of
    Left err -> Left ("Exception in +: " ++ err)
    Right vals -> Right (AInt (product vals))

builtinMod :: [Ast] -> Either String Ast
builtinMod args = case traverse foundInt args of
  Left err -> Left ("Exception in mod: " ++ err)
  Right [x, y] ->
    if y == 0 then Left "Exception in mod: undefined for 0"
              else Right (AInt (x `mod` y))
  Right [] -> Left ("Exception: incorrect argument count in call (mod)")
  Right _ -> Left ("Exception: incorrect argument count in call (mod " ++ show (AList args) ++ ")")

builtinDiv :: [Ast] -> Either String Ast
builtinDiv args = case traverse foundInt args of
  Left err -> Left ("Exception in div: " ++ err)
  Right [x, y] ->
    if y == 0 then Left "Exception in div: undefined for 0"
              else Right (AInt (x `div` y))
  Right [] -> Left ("Exception: incorrect argument count in call (div)")
  Right _ -> Left ("Exception: incorrect argument count in call (div " ++ show (AList args) ++ ")")

builtinEqual :: [Ast] -> Either String Ast
builtinEqual args = case traverse foundInt args of
  Left err -> Left ("Exception in eq?: " ++ err)
  Right [x, y] ->
    if x == y then Right (ABool True)
    else Right (ABool False)
  Right [] -> Left ("Exception: incorrect argument count in call (eq?)")
  Right _ -> Left ("Exception: incorrect argument count in call (eq? " ++ show (AList args) ++ ")")

isLower :: [Int] -> Ast
isLower [] = (ABool True)
isLower [_] = (ABool True)
isLower [val1, val2]
  | val1 < val2 = (ABool True)
  | otherwise = (ABool False)
isLower (val1:val2:rest) 
  | val1 < val2 = isLower (val2:rest)
  | otherwise = (ABool False)

builtinLower :: [Ast] -> Either String Ast
builtinLower [] = Left ("Exception: incorrect argument count in call (<)")
builtinLower args = case traverse foundInt args of
  Left err -> Left ("Exception in eq?: " ++ err)
  Right vals -> Right (isLower vals)

builtinGreater :: [Ast] -> Either String Ast
builtinGreater [] = Left ("Exception: incorrect argument count in call (<)")
builtinGreater args = case traverse foundInt args of
  Left err -> Left ("Exception in eq?: " ++ err)
  Right vals -> Right (isLower (reverse vals))

builtinIf :: [Ast] -> Either String Ast
builtinIf [_, expr1] = Right expr1
builtinIf [condition, expr1, expr2] = case foundBool condition of
  Left err -> Left ("Exception in eq?: " ++ err)
  Right False -> Right expr2
  Right True -> Right expr1
builtinIf [] = Left ("Exception: invalid syntax (if)")
builtinIf args = Left ("Exception: invalid syntax (if " ++ show (AList args) ++ ")")
