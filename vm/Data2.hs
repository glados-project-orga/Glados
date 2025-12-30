{- 
-- EPITECH PROJECT, 2025
-- data
-- File description:
-- part
-}

module Data2 (
        Op(..),
        Value(..),
        Action(..),
        HeapValue(..),
        Heap,
        Stack,
        Args,
        Insts,
        Env,
        Handle,
        Function
) where

import qualified Data.Map as Map


data Op = Add
        | Sub
        | Mul
        | Div
        | Eq
        | Less
        deriving (Show, Eq)


data Value = VInt  Int
          | VBool Bool
          | VOp Op
          | VFun Function
          | VHandle Handle
          deriving (Show, Eq)


data Action = IPush Value
           | ICall
           | IPushArg Int
           | IPushEnv String
           | IJumpIfFalse Int
           | IRet
           deriving (Show, Eq)

type Env = Map.Map String Function

type Stack = [Value]
type Insts = [Action]
type Args  = [Value]
type Function = [Action]