{- 
-- EPITECH PROJECT, 2025
-- Comparaison
-- File description:
-- instr
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module ComparInstr(
    compIfEq,
    compIfNe,
    compIfLt,
    compIfGt,
    compIfGe,
    compIfLe,

    compIfACmpEq,
    compIfACmpNe,

    compIfICmpEq,
    compIfICmpNe,
    compIfICmpLt,
    compIfICmpGt,
    compIfICmpGe,
    compIfICmpLe,

    compLCmp,
    compFCmpL,
    compFCmpG,
    compDCmpL,
    compDCmpG
) where


import Data

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


compIfICmpLt :: Int -> VMState -> Either String VMState
compIfICmpLt n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a < b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfICmpLt: expected two ints on stack"



compFCmpG :: VMState -> Either String VMState
compFCmpG st@VMState{stack, ip} =
  case stack of
    (VFloat b : VFloat a : rest) ->
      let r = if isNaN a || isNaN b
                then 1
                else if a < b then -1 else if a > b then 1 else 0
      in Right st { ip = ip + 1, stack = VInt r : rest }
    _ -> Left "fcmpg expects two floats"


compDCmpL :: VMState -> Either String VMState
compDCmpL st@VMState{stack, ip} =
  case stack of
    (VDouble b : VDouble a : rest) ->
      let r = if isNaN a || isNaN b
                then -1
                else if a < b then -1 else if a > b then 1 else 0
      in Right st { ip = ip + 1, stack = VInt r : rest }
    _ -> Left "dcmpl expects two doubles"


compIfNe :: Int -> VMState -> Either String VMState
compIfNe n st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v /= 0
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfNe: expected int on stack"


compIfLt :: Int -> VMState -> Either String VMState
compIfLt n st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v < 0
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfLt: expected int on stack"


compIfGe :: Int -> VMState -> Either String VMState
compIfGe n st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v >= 0
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfGe: expected int on stack"


compIfLe :: Int -> VMState -> Either String VMState
compIfLe n st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v <= 0
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfLe: expected int on stack"


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


compIfICmpGt :: Int -> VMState -> Either String VMState
compIfICmpGt n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a > b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfICmpGt: expected two ints on stack"


compIfICmpEq :: Int -> VMState -> Either String VMState
compIfICmpEq n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a == b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfICmpEq: expected two ints on stack"


compIfICmpNe :: Int -> VMState -> Either String VMState
compIfICmpNe n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a /= b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfICmpNe: expected two ints on stack"


compIfICmpGe :: Int -> VMState -> Either String VMState
compIfICmpGe n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a >= b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfICmpGe: expected two ints on stack"


compIfICmpLe :: Int -> VMState -> Either String VMState
compIfICmpLe n st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a <= b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfICmpLe: expected two ints on stack"


compLCmp :: VMState -> Either String VMState
compLCmp st@VMState{stack, ip} =
  case stack of
    (VLong b : VLong a : rest) ->
      let r = if a < b then -1 else if a > b then 1 else 0
      in Right st { ip = ip + 1, stack = VInt r : rest }
    _ -> Left "lcmp expects two longs"


compFCmpL :: VMState -> Either String VMState
compFCmpL st@VMState{stack, ip} =
  case stack of
    (VFloat b : VFloat a : rest) ->
      let r = if isNaN a || isNaN b
                then -1
                else if a < b then -1 else if a > b then 1 else 0
      in Right st { ip = ip + 1, stack = VInt r : rest }
    _ -> Left "fcmpl expects two floats"


compDCmpG :: VMState -> Either String VMState
compDCmpG st@VMState{stack, ip} =
  case stack of
    (VDouble b : VDouble a : rest) ->
      let r = if isNaN a || isNaN b
                then 1
                else if a < b then -1 else if a > b then 1 else 0
      in Right st { ip = ip + 1, stack = VInt r : rest }
    _ -> Left "dcmpg expects two doubles"
