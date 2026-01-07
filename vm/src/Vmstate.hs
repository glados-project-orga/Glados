
{- 
-- EPITECH PROJECT, 2025
-- vm
-- File description:
-- State
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Vmstate (
    compile
) where

import Data
import Data.Bits
import HeapInstr
import StackInstr
import qualified Data.Vector as V


execInstr :: Instr -> VMState -> Either String VMState

execInstr (IConstInt n) st@VMState{stack, ip} =
    Right st {ip = (ip + 1), stack = (VInt n : stack)}

execInstr (ILoadInt n) st = stackInstrLoadInt n st

execInstr (IStoreInt n) st = stackInstrStoreInt n st

execInstr (IPop) st = stackInstrPop 1 st

execInstr (IPop2) st = stackInstrPop 2 st

execInstr (IDup) st =  stackInstrDup Dup st

execInstr (IDupX1) st = stackInstrDup DupX1 st

execInstr (IDupX2) st =  stackInstrDup DupX2 st

execInstr (IDup2X1) st = stackInstrDup Dup2X1 st
    
execInstr (IDup2X2) st = stackInstrDup Dup2X2 st

execInstr (ISwap) st = stackInstrSwap st

execInstr (INop) st = Right st

execInstr (INewArray) st = heapInstr_1 st

execInstr (IALoad) st = heapInstr_2 st

execInstr (IAStore) st = heapInstr_3 st

execInstr (IArrayLength) st = heapInstr_4 st

execInstr _  _ = Left "Invalid instruction or not yet implemented"

-- execInstr (IMulInt) st@VMState{stack, ip} =
--     case stack of
--         [] -> Left "Empty Stack  in IMulInt"
--         (VInt i2: VInt i1: rest) -> 
--             let newRet = i1 * i2
--             in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
--         _ -> Left "IMulInt expects two integers on the stack"


-- execInstr (IDivInt) st@VMState{stack, ip} = 
--     case stack of
--         [] -> Left "Empty Stack  in IDivInt"
--         (VInt i2: VInt i1: rest) -> 
--             let newRet = i1 `div` i2
--             in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
--         _ -> Left "IDivInt expects two integers on the stack"


-- execInstr (ISubInt) st@VMState{stack, ip} =
--     case stack of
--         [] -> Left "Empty Stack  in ISubInt"
--         (VInt i2: VInt i1: rest) -> 
--             let newRet = i1 - i2
--             in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
--         _ -> Left "ISubInt expects two integers on the stack" 


-- execInstr (IAddInt) st@VMState{stack, ip} =
--     case stack of
--         [] -> Left "Empty Stack  in IAddInt"
--         (VInt i2: VInt i1: rest) -> 
--             let newRet = i1 + i2
--             in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
--         _ -> Left "IAddInt expects two integers on the stack" 


-- execInstr (INegInt) st@VMState{stack, ip} =
--     case stack of
--         [] -> Left "Empty Stack  in IAddInt"
--         (VInt i:rest) -> Right st { ip = ip + 1, stack = VInt (-i) : rest }
--         _ -> Left "IAddInt expects one integers on the stack" 


-- execInstr (IOrInt) st@VMState{stack, ip} =
--     case stack of
--         [] -> Left "Empty Stack  in IOrInt"
--         (VInt i2: VInt i1: rest) -> 
--             let newRet = i1 .|. i2
--             in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
--         _ -> Left "IOrInt expects two integers on the stack" 


-- execInstr (IXorInt) st@VMState{stack, ip} =
--     case stack of
--         [] -> Left "Empty Stack  in IXorInt"
--         (VInt i2: VInt i1: rest) -> 
--             let newRet = i1 `xor` i2
--             in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
--         _ -> Left "IXorInt expects two integers on the stack" 


-- execInstr (IShrInt) st@VMState{stack, ip} =
--     case stack of
--         [] -> Left "Empty Stack  in IShrInt"
--         (VInt i2: VInt i1: rest) ->
--             let s = i2 .&. 0x1F
--                 newVal = i1 `shiftR` s
--             in Right st {ip = ip + 1, stack = (VInt newVal : rest)}
--         _ -> Left "IShrInt expects two integers on the stack" 


-- execInstr (IShlInt) st@VMState{stack, ip} =
--     case stack of
--         [] -> Left "Empty Stack  in IShlInt"
--         (VInt i2: VInt i1: rest) ->
--             let s = i2 .&. 0x1F
--                 newVal = i1 `shiftL` s
--             in Right st {ip = ip + 1, stack = (VInt newVal : rest)}
--         _ -> Left "IShlInt expects two integers on the stack" 


-- execInstr (IIfEq n) st@VMState{stack, ip} =
--     case stack of
--         (VInt v : rest) ->
--             if v == 0
--                 then Right st { ip = ip + n, stack = rest }
--                 else Right st { ip = ip + 1, stack = rest }
--         _ -> Left "IIfEq expects an integer on the stack"


-- execInstr (IIfACmpEq n) st@VMState{stack, ip} =
--     case stack of
--         (VInt b : VInt a : rest) ->
--             if a == b
--                 then Right st { ip = ip + n, stack = rest }
--                 else Right st { ip = ip + 1, stack = rest }
--         _ -> Left "if_acmpeq expects two references"


-- execInstr (IIfACmpNe n) st@VMState{stack, ip} =
--     case stack of
--         (VInt b : VInt a : rest) ->
--             if a /= b
--                 then Right st { ip = ip + n, stack = rest }
--                 else Right st { ip = ip + 1, stack = rest }
--         _ -> Left "if_acmpne expects two references"






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
