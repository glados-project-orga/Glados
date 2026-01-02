{- 
-- EPITECH PROJECT, 2025
-- vm
-- File description:
-- State
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


import Data2
import qualified Data.Vector as V

exec :: VMState -> Either String VMState
exec st@VMState{stack, locals, ip, code, heap, frames} =
    case code V.!? ip of
        Nothing ->
            Left ("Invalid instruction ip: " ++ show ip)
        Just instr ->
            case instr of
                IConstInt n -> Right st {ip = ip + 1, stack = (VInt n : stack)}
    
                ILoadInt n ->
                    case locals V.!? n of
                        Just val ->
                            Right st {ip = ip +1, stack = (val : stack)}
                        Nothing ->
                            Left "Invalid index for local"
                
                IStoreInt n ->
                    case stack of
                        [] -> Left "Stack underflow in IStoreInt"
                        (first:rest) ->
                            case locals V.!? n of 
                                Nothing -> Left ("Invalid local index: " ++ show n) 
                                Just _ -> 
                                    let newLocals = locals V.// [(n, first)] 
                                    in Right st {ip = ip + 1, stack = rest, locals = newLocals}
                                IMulInt ->
                    case stack of
                        [] -> Left "Empty Stack  in IMulInt"
                        (VInt i2: VInt i1: rest) -> 
                            let newRet = i1 * i2
                            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
                        _ -> Left "Type error or insufficient stack in IMulInt"

                IDivInt ->
                    case stack of
                        [] -> Left "Empty Stack  in IDivInt"
                        (VInt i2: VInt i1: rest) -> 
                            let newRet = i1 `div` i2
                            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
                        _ -> Left "Type error or insufficient stack in IDivInt"

                ISubInt ->
                    case stack of
                        [] -> Left "Empty Stack  in ISubInt"
                        (VInt i2: VInt i1: rest) -> 
                            let newRet = i1 - i2
                            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
                        _ -> Left "Type error or insufficient stack in ISubInt" 
                
                IAddInt ->
                    case stack of
                        [] -> Left "Empty Stack  in IAddInt"
                        (VInt i2: VInt i1: rest) -> 
                            let newRet = i1 + i2
                            in Right st {ip = ip + 1, stack = (VInt newRet:rest)}
                        _ -> Left "Type error or insufficient stack in IAddInt"
        
                _ -> Left ("Instruction not implemented: " ++ show instr)
