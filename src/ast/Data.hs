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
         | ABool Bool
         | AList [Ast]
         | ACall Ast [Ast]
         | ADefine String Ast
         | ALamdba [String] Ast
         deriving (Eq)

instance Show Ast where
    show (AInt n) = show n
    show (ASymbol s) = s
    show (ABool b) = show b
    show (AList list) = (formatList list)
    show (ACall f args) = "(" ++ show f ++ " " ++ concatMap (\x -> show x ++ " ") args ++ ")"
    show (ADefine name expr) = "(define " ++ name ++ " " ++ show expr ++ ")"
    show (ALamdba params body) = "(lambda (" ++ concatMap (\x -> x ++ " ") params ++ ") " ++ show body ++ ")"

formatList :: [Ast] -> String
formatList [] = ""
formatList [x] = show x
formatList (x:xs) = show x ++ " " ++ formatList xs