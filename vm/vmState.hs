{- 
-- EPITECH PROJECT, 2025
-- vm
-- File description:
-- State
-}



module Vmstate (
    compile
) where

import Data2
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
        _ -> Left "Type error or insufficient stack in IMulInt"


execInstr (IDivInt) st@VMState{stack, ip} = 
    case stack of
        [] -> Left "Empty Stack  in IDivInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 `div` i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "Type error or insufficient stack in IDivInt"


execInstr (ISubInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in ISubInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 - i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "Type error or insufficient stack in ISubInt" 

           
execInstr (IAddInt) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack  in IAddInt"
        (VInt i2: VInt i1: rest) -> 
            let newRet = i1 + i2
            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
        _ -> Left "Type error or insufficient stack in IAddInt" 


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
        [] -> Left "Empty Stack"
        (_: rest) -> Right st {ip = ip + 1, stack = rest}


execInstr (IDup) st@VMState{stack, ip} =
    case stack of
        [] -> Left "Empty Stack"
        (first: rest) -> 
            let val = first
                newStack = (val:first:rest)
            in Right st {ip = ip + 1, stack = newStack}


execInstr _  _ = Left "Invalid isntruction or not yet implemented"



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
