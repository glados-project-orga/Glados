{- 
-- EPITECH PROJECT, 2025
-- data
-- File description:
-- part
-}

module Data2 (
        Heap,
        Stack,
        Value(..),
        Instr(..),
        Handle,
        VMState(..),
        HeapValue(..)
        -- Action(..),
        -- Op(..),
        -- Function
        -- Args,
        -- Insts,
        -- Env,
) where

import qualified Data.Map as Map
import qualified Data.Vector as V

-- data Op = Add
--         | Sub
--         | Mul
--         | Div
--         | Eq
--         | Less
--         deriving (Show, Eq)

data Value = VInt  Int
          | VBool Bool
          | VString String
          | VHandle Handle
          deriving (Show, Eq)
        --   | VOp Op
        --   | VFun Function
        --   | VNull


-- data Action = IPush Value
--            | ICall
--            | IPushArg Int
--            | IPushEnv String
--            | IJumpIfFalse Int
--            | IRet
--            deriving (Show, Eq)

-- type Env = Map.Map String Function
-- type Insts = [Action]
-- type Args  = [Value]
-- type Function = [Action]


type Stack = [Value]

type Handle = Int

data HeapValue = HObject (Map.Map String Value)
               |HArray (V.Vector Value) 
               deriving(Show, Eq)

type Heap = V.Vector HeapValue


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
           locals :: V.Vector Value,
           ip     :: Int,
           code   :: V.Vector Instr,
           heap   :: Heap,
           frames :: [Frame]
        } deriving(Show)
