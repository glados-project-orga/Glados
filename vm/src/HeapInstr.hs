{- 
-- EPITECH PROJECT, 2025
-- heap
-- File description:
-- hs
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HeapInstr (
      heapInstrALoad,
      heapInstrAStore,
      heapInstrNewArray,
      heapInstrArrayLength,
      heapInstrNew,
      heapInstrGetField,
      heapInstrPutField
) where

import Data
import qualified Data.Map as Map
import qualified Data.Vector as V

heapInstrNewArray:: VMState -> Either String VMState
heapInstrNewArray st@VMState{stack, ip, heap} =
  case stack of
      (VInt size : rest) ->
        if size < 0 then Left "INewArray: negative array size"
        else
          let handle = V.length heap
              newArray = HArray (V.replicate size (VInt 0))
              newHeap  = V.snoc heap newArray
          in Right st {ip = ip + 1, heap = newHeap, stack = (VInt handle : rest)}
      _ -> Left "INewArray expects an integer size on the stack"


heapInstrALoad:: VMState -> Either String VMState
heapInstrALoad st@VMState{stack, ip, heap} =
  case stack of
      (VInt v2 : VInt v1 : rest) ->
        case heap V.!? v1 of
            Just (HArray arr) ->
              case arr V.!? v2 of
                  Just val -> Right st {ip = ip + 1, stack = (val : rest)}
                  Nothing -> Left "IALoad: array index out of bounds"
            _ -> Left "IALoad: reference is not an array"
      _ -> Left "IALoad expects (index, arrayRef) on the stack"


heapInstrAStore:: VMState -> Either String VMState
heapInstrAStore st@VMState{stack, ip, heap} =
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


heapInstrArrayLength:: VMState -> Either String VMState
heapInstrArrayLength st@VMState{stack, ip, heap} =
  case stack of
      (VInt ref : rest) ->
        case heap V.!? ref of
            Just (HArray arr) ->
                let len = V.length arr
                in Right st { ip = ip + 1 , stack = (VInt len : rest)}
            _ -> Left "IArrayLength: reference is not an array"
      _ -> Left "IArrayLength expects an array reference ( VInt) on the stack"


heapInstrNew :: String -> VMState -> Either String VMState
heapInstrNew _className st@VMState{stack, ip, heap} =
    let handle = V.length heap
        newObj = HObject Map.empty
        newHeap = V.snoc heap newObj
    in Right st { ip = ip + 1, stack = VHandle handle : stack, heap = newHeap }


heapInstrGetField :: String -> VMState -> Either String VMState
heapInstrGetField fieldName st@VMState{stack, ip, heap} =
    case stack of
        (VHandle h : rest) ->
            case heap V.!? h of
                Just (HObject fields) ->
                    case Map.lookup fieldName fields of
                        Just val -> Right st {ip = ip + 1, stack = val : rest}
                        Nothing  -> Left ("Field not found: " ++ fieldName)
                Just (HArray _) -> Left "getfield: expected object, got array"
                Nothing -> Left ("Invalid heap handle: " ++ show h)
        [] -> Left "Stack underflow in IGetField"
        _ -> Left "IGetField expects an object reference on the stack"


heapInstrPutField :: String -> VMState -> Either String VMState
heapInstrPutField fieldName st@VMState{stack, ip, heap} =
    case stack of
        (val : VHandle h : rest) ->
            case heap V.!? h of
                Just (HObject fields) ->
                    let newFields = Map.insert fieldName val fields
                        newHeap = heap V.// [(h, HObject newFields)]
                    in Right st {ip = ip + 1, stack = rest, heap = newHeap}
                Just (HArray _) -> Left "putfield: expected object, got array"
                Nothing -> Left ("Invalid heap handle: " ++ show h)
        [_] -> Left "Stack underflow in IPutField (need value and object ref)"
        [] -> Left "Stack underflow in IPutField"
        _ -> Left "IPutField expects value and object reference on the stack"
