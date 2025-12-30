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


type Handle = Int

data HeapValue = HObject (Map.Map String Value)
               |HArray [Value] 
               deriving(Show, Eq)

type Heap = Map.Map Handle HeapValue


data Instr = IConstInt Int
           | ILoadInt Int
           | IStoreInt Int
           | IAddInt
           | ISubInt
           | IMulInt
           | IDivInt
           | IPop
           | IDup
   
           | IGoto Int
           | IIfEq Int
           | IIfGt Int
           | IIfLt Int
           | IIfICmpGt Int
           | IIfICmpLt Int
   
           | IInvokeStatic String
           | IInvokeVirtual String
           | IInvokeSpecial String
           | IReturn
           | IReturnInt
   
           | INew String
           | IGetField String
           | IPutField String
   
           | INewArray
           | IALoad
           | IAStore
           | IArrayLength
           deriving (Show, Eq)

data Frame = Frame
    { fLocals :: [Value]
    , fIP     :: Int
    } deriving (Show)


data VMState = VMState
        {  stack  :: Stack,
           locals :: [Value],
           ip     :: Int,
           code   :: [Instr],
           heap   :: Heap,
           frames :: [Frame]
        } deriving(Show)