{- 
-- EPITECH PROJECT, 2025
-- data
-- File description:
-- part
-}

module Data (
        Heap,
        Stack,
        Handle,
        IntOp(..),
        Value(..),
        Instr(..),
        WhatDup(..),
        VMState(..),
        StackIns(..),
        HeapValue(..),
        Function(..),
        Frame(..)

) where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Int (Int64)

data Value = VInt  Int
          | VBool Bool
          | VString String
          | VFloat Float
          | VDouble Double
          | VLong Int64
          | VNull
          | VHandle Handle
          deriving (Show, Eq)

type Stack = [Value]

type Handle = Int

data WhatDup = Dup | Dup2 | DupX1 | DupX2 | Dup2X1 | Dup2X2

data HeapValue = HObject (Map.Map String Value)
               |HArray (V.Vector Value) 
               deriving (Show, Eq)

type Heap = V.Vector HeapValue

data IntOp = IAddInt 
           | ISubInt | IMulInt | IDivInt | IRemInt
           | INegInt | IAndInt | IOrInt | IXorInt
           | IShlInt | IShrInt
           deriving (Show, Eq)

data StackIns = IPop | IDup | INop | ISwap | IDup2
           | IPop2 | IDupX1 | IDupX2 | IDup2X1 | IDup2X2
           deriving (Show, Eq)

data Instr = IConstInt Int
           | ILdc Int
           | ILoadInt Int
           | IStoreInt Int
        
           | IOpInt IntOp 
           | IStck StackIns
           
           | IIfEq Int | IIfNe Int | IIfLt Int | IIfGe Int
           | IIfGt Int | IIfLe Int | IIfACmpEq Int | IIfACmpNe Int           
           | IIfICmpEq Int | IIfICmpNe Int | IIfICmpLt Int | IIfICmpGe Int
           | IIfICmpGt Int | IIfICmpLe Int | ILcmp | IFcmpl
           | IFcmpg | IDcmpl | IDcmpg

           -------------------------------

           | IGoto Int

           | IInvokeStatic String

           | IReturn | IReturnInt
   
           | INew String | IGetField String | IPutField String

           | INewArray | IALoad | IAStore | IArrayLength
           deriving (Show, Eq)


data Frame = Frame
    { fLocals :: [Value]
    , fIP     :: Int
    , fFunction :: String
    } deriving (Show)


data Function = Function
    { funcName :: String
    , funcCode :: V.Vector Instr
    } deriving (Show, Eq)


data VMState = VMState
        {  stack        :: Stack,
           locals       :: V.Vector Value,
           ip           :: Int,
           functions    :: Map.Map String Function,
           currentFunc  :: String,
           constPool    :: V.Vector Value,
           heap         :: Heap,
           frames       :: [Frame]
        } deriving(Show)
