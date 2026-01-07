{- 
-- EPITECH PROJECT, 2025
-- heap
-- File description:
-- hs
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HeapInstr (
      heapInstr_1,
      heapInstr_2,
      heapInstr_3,
      heapInstr_4
) where

import Data
import qualified Data.Map as Map
import qualified Data.Vector as V

heapInstr_1:: VMState -> Either String VMState
heapInstr_1 st@VMState{stack, ip, heap} =
  case stack of
      (VInt size : rest) ->
        if size < 0 then Left "INewArray: negative array size"
        else
          let handle = V.length heap
              newArray = HArray (V.replicate size (VInt 0))
              newHeap  = V.snoc heap newArray
          in Right st { ip = ip + 1, heap = newHeap, stack = (VInt handle : rest)}
      _ -> Left "INewArray expects an integer size on the stack"


heapInstr_2:: VMState -> Either String VMState
heapInstr_2 st@VMState{stack, ip, heap} =
  case stack of
      (VInt v2 : VInt v1 : rest) ->
        case heap V.!? v1 of
            Just (HArray arr) ->
              case arr V.!? v2 of
                  Just val -> Right st { ip = ip + 1, stack = (val : rest)}
                  Nothing -> Left "IALoad: array index out of bounds"
            _ -> Left "IALoad: reference is not an array"
      _ -> Left "IALoad expects (index, arrayRef) on the stack"


heapInstr_3:: VMState -> Either String VMState
heapInstr_3 st@VMState{stack, ip, heap} =
    case stack of
        [] -> Left "Empty Stack in AStore"
        (v3 : VInt v2: VInt v1: rest) ->
            case heap V.!? v1 of
                Just (HArray tabb) ->
                    if v2 < 0 || v2 >= V.length tabb
                      then Left "IAStore: array index out of bounds"
                    else
                        let newHArray = tabb V.// [(v2, v3)]
                            newHeap = heap V.// [(v1, HArray newHArray)]
                        in Right st {ip = ip + 1, heap = newHeap, stack = rest}
                _ ->  Left "IAStore: reference is not an array"
        _ -> Left "IAStore expects (value, index, arrayRef) on the stack" 


heapInstr_4:: VMState -> Either String VMState
heapInstr_4 st@VMState{stack, ip, heap} =
  case stack of
      (VInt ref : rest) ->
        case heap V.!? ref of
            Just (HArray arr) ->
                let len = V.length arr
                in Right st { ip = ip + 1 , stack = (VInt len : rest)}
            _ -> Left "IArrayLength: reference is not an array"
      _ -> Left "IArrayLength expects an array reference ( VInt) on the stack"

