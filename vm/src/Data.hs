{- 
-- EPITECH PROJECT, 2025
-- data
-- File description:
-- part
-}

module Data (
        Heap,
        Stack,
        Value(..),
        Instr(..),
        Handle,
        VMState(..),
        HeapValue(..)

) where

import qualified Data.Map as Map
import qualified Data.Vector as V

data Value = VInt  Int
          | VBool Bool
          | VString String
          | VHandle Handle
          deriving (Show, Eq)

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
           | IRemInt 
           | INegInt 
           | IOrInt 
           | IXorInt 
           | IShlInt 
           | IShrInt 

           | IPop
           | IDup
           | INop
           | ISwap
           | IDup2 
           | IPop2
           | IDupX1 
           | IDupX2
           | IDup2X1 
           | IDup2X2

           -------------------------------

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
