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
        Frame(..),
        IntOp(..),
        Value(..),
        Instr(..),
        LongOp(..),
        ConvOp(..),
        FloatOp(..),
        WhatDup(..),
        VMState(..),
        StackIns(..),
        DoubleOp(..),
        Function(..),
        HeapValue(..),
        StackCharg(..)
) where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Int (Int64)

data Value = VInt  Int
          | VBool Bool
          | VChar Char
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

data DoubleOp = DAddDouble | DSubDouble | DMulDouble 
           | DDivDouble | DRemDouble | DNegDouble
           deriving (Show, Eq)

data FloatOp = FAddFloat | FSubFloat | FMulFloat 
            | FDivFloat | FRemFloat | FNegFloat
           deriving (Show, Eq)

data LongOp = LAddLong 
           | LSubLong | LMulLong | LDivLong | LRemLong
           | LNegLong | LAndLong | LOrLong | LXorLong
           | LShlLong | LShrLong
           deriving (Show, Eq)

data ConvOp = I2c | I2l | I2f | I2d
            | L2i | L2f | L2d
            | F2i | F2l | F2d
            | D2i | D2l | D2f
            | C2i
            deriving (Show, Eq)
        
data StackIns = IPop | IDup | INop | ISwap | IDup2
           | IPop2 | IDupX1 | IDupX2 | IDup2X1 | IDup2X2
           deriving (Show, Eq)

data StackCharg =  ILoadInt Int 
                | IStoreInt Int | IConstInt Int
                | ILoadFloat Int | IStoreFloat Int | IConstFloat Float
                | ILoadLong Int | IStoreLong Int | IConstLong Int64
                | ILoadDouble Int | IStoreDouble Int | IConstDouble Double
                | ILoadChar Int | IStoreChar Int | IConstChar Char | IConstString String
                | ALoad Int  | AStore  Int
           deriving (Show, Eq)

data Instr = ILdc Int

           | IOpInt IntOp 
           | IOpFloat FloatOp
           | IOpDouble DoubleOp
           | IOpLong LongOp

           | IConv ConvOp
           | IStck_1 StackCharg
           | IStck StackIns
           
           | IIfEq Int | IIfNe Int | IIfLt Int | IIfGe Int
           | IIfGt Int | IIfLe Int | IIfACmpEq Int | IIfACmpNe Int           
           | IIfICmpEq Int | IIfICmpNe Int | IIfICmpLt Int | IIfICmpGe Int
           | IIfICmpGt Int | IIfICmpLe Int | ILcmp | IFcmpl
           | IFcmpg | IDcmpl | IDcmpg

           | IGoto Int
           | IInvokeStatic String
           | IReturn | IReturnInt | IReturnDouble | IReturnFloat | IReturnLong | IReturnA
           | INew String | IGetField String | IPutField String

           | INewArray | IALoad | IAStore | IArrayLength
           | IIinc Int Int
           | IInvokeWrite Int
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
