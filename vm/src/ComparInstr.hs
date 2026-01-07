{- 
-- EPITECH PROJECT, 2025
-- Comparaison
-- File description:
-- instr
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data
import Data.Bits
import HeapInstr
import StackInstr
import qualified Data.Vector as V


compIfEq :: Int -> VMState -> Either String VMState
compIfEq n st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v == 0
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfEq: expected int on stack"


compIfGt :: Int -> VMState -> Either String VMState
compIfGt n st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v > 0
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfGt: expected int on stack"


compIfLt :: Int -> VMState -> Either String VMState
compIfLt n st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v < 0
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfLt: expected int on stack"


compIfICmpGt :: Int -> VMState -> Either String VMState
compIfICmpGt n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a > b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfICmpGt: expected two ints on stack"


compIfICmpLt :: Int -> VMState -> Either String VMState
compIfICmpLt n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a < b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfICmpLt: expected two ints on stack"


compIfACmpEq :: Int -> VMState -> Either String VMState
compIfACmpEq n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a == b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfACmpEq: expected two refs on stack"


compIfACmpNe :: Int -> VMState -> Either String VMState
compIfACmpNe n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a /= b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfACmpNe: expected two refs on stack"