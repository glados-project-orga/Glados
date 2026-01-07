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
compIfEq offset st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v == 0
                then Right st { ip = ip + offset, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfEq: expected int on stack"


compIfGt :: Int -> VMState -> Either String VMState
compIfGt offset st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v > 0
                then Right st { ip = ip + offset, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfGt: expected int on stack"


compIfLt :: Int -> VMState -> Either String VMState
compIfLt offset st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v < 0
                then Right st { ip = ip + offset, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfLt: expected int on stack"