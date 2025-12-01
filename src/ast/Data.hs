{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- Data.hs
-}

module Data (
  Ast(..),
  SExpr(..)
) where

data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show, Eq)

data Ast = AInt Int
         | ASymbol String
         | AList [Ast]
         | ACall Ast [Ast]
         | ADefine String Ast
         | ALamdba [String] Ast
         deriving (Show, Eq)