{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{- 
-- EPITECH PROJECT, 2025
-- vm
-- File description:
-- State
-}

module Vmstate (
    compile
) where

import Data
import Data.Bits
import qualified Data.Vector as V

execInstr :: Instr -> VMState -> Either String VMState

execInstr (IConstInt n) st@VMState{stack, ip} =
    Right st { ip = ip + 1, stack = (VInt n : stack)}

execInstr (ILoadInt n) st@VMState{stack, ip, locals} =
    case locals V.!? n of
        Just val -> Right st {ip = ip + 1, stack = (val : stack)}
        Nothing  -> Left "Invalid index for local"


execInstr (IStoreInt n) st@VMState{stack, ip, locals} =
    case stack of
        [] -> Left "Stack underflow in IStoreInt"
        (first:rest) ->
            case locals V.!? n of 
                Nothing -> Left ("Invalid local index: " ++ show n) 
                Just _ -> 
                    let newLocals = locals V.// [(n, first)] 
                    in Right st {ip = ip + 1, stack = rest, locals = newLocals}

        
execInstr (IMulInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in IMulInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 * i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "IMulInt expects two integers on the stack"


execInstr (IDivInt) st@VMState{stack, ip} = 
    case stack of
        [] -> Left "Empty Stack  in IDivInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 `div` i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "IDivInt expects two integers on the stack"


execInstr (ISubInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in ISubInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 - i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "ISubInt expects two integers on the stack" 


execInstr (IAddInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in IAddInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 + i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "IAddInt expects two integers on the stack" 


execInstr (INegInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in IAddInt"
        (VInt i:rest) -> Right st { ip = ip + 1, stack = VInt (-i) : rest }
        _ -> Left "IAddInt expects one integers on the stack" 


execInstr (IOrInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in IOrInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 .|. i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "IOrInt expects two integers on the stack" 


execInstr (IXorInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in IXorInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 `xor` i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "IXorInt expects two integers on the stack" 


execInstr (IShrInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in IShrInt"
        (VInt i2: VInt i1: rest) ->
            let s = i2 .&. 0x1F
                newVal = i1 `shiftR` s
            in Right st {ip = ip + 1, stack = (VInt newVal : rest)}
        _ -> Left "IShrInt expects two integers on the stack" 


execInstr (IShlInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in IShlInt"
        (VInt i2: VInt i1: rest) ->
            let s = i2 .&. 0x1F
                newVal = i1 `shiftL` s
            in Right st {ip = ip + 1, stack = (VInt newVal : rest)}
        _ -> Left "IShlInt expects two integers on the stack" 


execInstr (IPop) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack in IPop"
        (_: rest) -> Right st {ip = ip + 1, stack = rest}


execInstr (IPop2) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack in in IPop2"
        (_:_:rest) -> Right st {ip = ip + 1, stack = rest}
        _ -> Left "IPop2 expects at least two stack values"


execInstr (IDup) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack in IDup"
        (first: rest) -> 
            let newStack = (first:first:rest)
            in Right st {ip = ip + 1, stack = newStack}


execInstr (IDupX1) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack in IDupX1"
        (second: first: rest) -> 
            let newStack = (first:second:first:rest)
            in Right st {ip = ip + 1, stack = newStack}
        _ -> Left "IDupX1 expects at least two stack values"


execInstr (IDupX2) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack in IDupX2"
        (third:second:first:rest) -> 
            let newStack = (first:third:second:first:rest)
            in Right st {ip = ip + 1, stack = newStack}
        _ -> Left "IDupX2 expects at least three stack values"


execInstr (IDup2X1) st@VMState{stack, ip} =
    case stack of
        (v1:v2:v3:rest) ->
            let newStack = v2:v1:v3:v2:v1:rest
            in Right st { ip = ip + 1, stack = newStack }
        _ -> Left "IDup2X1 expects at least 3 stack values"


execInstr IDup2X2 st@VMState{stack, ip} =
    case stack of
        (v1:v2:v3:v4:rest) ->
            let newStack = v2:v1:v4:v3:v2:v1:rest
            in Right st { ip = ip + 1, stack = newStack }
        _ -> Left "IDup2X2 expects at least 4 stack values"


execInstr (ISwap) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack in ISwap"
        (second: first: rest) ->
            let newStack = (first:second:rest)
            in Right st {ip = ip + 1, stack = newStack}
        _ -> Left "ISwap expects at least two stack values"


execInstr (INop) st = Right st

execInstr (INewArray) st@VMState{stack, ip, heap} =
    case stack of
        (VInt size : rest) ->
            if size < 0 then
                Left "INewArray: negative array size"
            else
                let handle   = V.length heap
                    newArray = HArray (V.replicate size (VInt 0))
                    newHeap  = V.snoc heap newArray
                in Right st { ip = ip + 1, heap = newHeap , stack = (VInt handle : rest)}

        _ -> Left "INewArray expects an integer size on the stack"


execInstr (IALoad) st@VMState{stack, ip, heap} =
    case stack of
        (VInt v2 :  VInt v1 : rest) ->
            case heap V.!? v1 of
                Just (HArray arr) ->
                    case arr V.!? v2 of
                        Just val ->
                            Right st { ip = ip + 1, stack = (val : rest)}
                        Nothing -> Left "IALoad: array index out of bounds"
                _ -> Left "IALoad: reference is not an array"
        _ -> Left "IALoad expects (index, arrayRef) on the stack"


execInstr (IAStore) st@VMState{stack, ip, heap} =
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


execInstr (IArrayLength) st@VMState{stack, ip, heap} =
    case stack of
        (VInt ref : rest) ->
            case heap V.!? ref of
                Just (HArray arr) ->
                    let len = V.length arr
                    in Right st { ip = ip + 1 , stack = (VInt len : rest)}
                _ -> Left "IArrayLength: reference is not an array"
        _ -> Left "IArrayLength expects an array reference ( VInt) on the stack"


execInstr (IIfEq n) st@VMState{stack, ip} =
    case stack of
        (VInt v : rest) ->
            if v == 0
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "IIfEq expects an integer on the stack"


execInstr (IIfACmpEq n) st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a == b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "if_acmpeq expects two references"


execInstr (IIfACmpNe n) st@VMState{stack, ip} =
    case stack of
        (VInt b : VInt a : rest) ->
            if a /= b
                then Right st { ip = ip + n, stack = rest }
                else Right st { ip = ip + 1, stack = rest }
        _ -> Left "if_acmpne expects two references"


execInstr _  _ = Left "Invalid instruction or not yet implemented"




exec :: VMState -> Either String VMState
exec st@VMState{stack, locals, ip, code, heap, frames}
    | Just instr <- code V.!? ip = execInstr instr st
    | otherwise = Left ("Invalid instruction ip: " ++ show ip)


compile :: VMState -> Either String VMState
compile vmst =
  if ip vmst >= V.length (code vmst)
    then Right vmst
    else
      case exec vmst of
        Left err  -> Left err
        Right newvmst -> compile newvmst
