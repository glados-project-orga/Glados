{- 
-- EPITECH PROJECT, 2025
-- Conversion
-- File description:
-- Type conversion instructions
-}

{-# LANGUAGE NamedFieldPuns #-}

module ConvInstr (
    convOp
) where

import Data
import Data.Char (chr, ord)

convOp :: ConvOp -> VMState -> Either String VMState
convOp op st@VMState{stack, ip} =
    case (op, stack) of
        (I2c, (VInt i : rest)) ->
            let c = chr (i `mod` 128)
            in Right st { ip = ip + 1, stack = VChar c : rest }

        (C2i, (VChar c : rest)) ->
            Right st { ip = ip + 1, stack = VInt (ord c) : rest }

        (I2l, (VInt i : rest)) ->
            Right st { ip = ip + 1, stack = VLong (fromIntegral i) : rest }

        (I2f, (VInt i : rest)) ->
            Right st { ip = ip + 1, stack = VFloat (fromIntegral i) : rest }

        (I2d, (VInt i : rest)) ->
            Right st { ip = ip + 1, stack = VDouble (fromIntegral i) : rest }

        (L2i, (VLong l : rest)) ->
            Right st { ip = ip + 1, stack = VInt (fromIntegral l) : rest }

        (L2f, (VLong l : rest)) ->
            Right st { ip = ip + 1, stack = VFloat (fromIntegral l) : rest }

        (L2d, (VLong l : rest)) ->
            Right st { ip = ip + 1, stack = VDouble (fromIntegral l) : rest }

        (F2i, (VFloat f : rest)) ->
            Right st { ip = ip + 1, stack = VInt (truncate f) : rest }

        (F2l, (VFloat f : rest)) ->
            Right st { ip = ip + 1, stack = VLong (truncate f) : rest }

        (F2d, (VFloat f : rest)) ->
            Right st { ip = ip + 1, stack = VDouble (realToFrac f) : rest }

        (D2i, (VDouble d : rest)) ->
            Right st { ip = ip + 1, stack = VInt (truncate d) : rest }

        (D2l, (VDouble d : rest)) ->
            Right st { ip = ip + 1, stack = VLong (truncate d) : rest }

        (D2f, (VDouble d : rest)) ->
            Right st { ip = ip + 1, stack = VFloat (realToFrac d) : rest }

        _ -> Left $ "convOp: invalid operand for " ++ show op
