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



execInstr IDup2X1 st@VMState{stack, ip} =
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
