{- 
-- EPITECH PROJECT, 2025
-- Operation
-- File description:
-- instr
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module ArithmInt (
    intArith
) where

import Data
import Data.Bits
import Data.Fixed

intArith :: IntOp -> VMState -> Either String VMState
intArith op st@VMState{stack, ip} =

    case (op, stack) of
        (INegInt, (VInt i : rest)) ->
            Right st { ip = (ip + 1) , stack = (VInt (-i):rest)}

        (_, (VInt i2 : VInt i1 : rest)) ->
            let result = case op of
                            IAddInt  -> i1 + i2
                            ISubInt  -> i1 - i2
                            IMulInt  -> i1 * i2
                            IDivInt  -> i1 `div` i2
                            IRemInt  -> i1 `mod` i2
                            IAndInt  -> i1 .&. i2
                            IOrInt  -> i1 .|. i2
                            IXorInt  -> xor i1 i2
                            IShlInt -> 
                                let s = i2 .&. 0x1F 
                                in i1 `shiftL` s 
                            IShrInt ->
                                let s = i2 .&. 0x1F 
                                in i1 `shiftR` s
            in Right st { ip = ip + 1, stack = (VInt result : rest)}
        _ -> Left "intArith: invalid operands"



doubleArith :: DoubleOp -> VMState -> Either String VMState
doubleArith op st@VMState{stack, ip} =
    case (op, stack) of
        (DNegDouble, (VDouble i : rest)) ->
            Right st { ip = ip + 1, stack = VDouble (-i) : rest }

        (_, (VDouble i2 : VDouble i1 : rest)) ->
            let result = case op of
                    DAddDouble -> i1 + i2
                    DSubDouble -> i1 - i2
                    DMulDouble -> i1 * i2
                    DDivDouble -> i1 / i2
                    DRemDouble -> i1 `mod'` i2
            in Right st { ip = ip + 1, stack = (VDouble result : rest) }

        _ -> Left "doubleArith: invalid operands"
