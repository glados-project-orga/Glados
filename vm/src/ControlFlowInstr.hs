{- 
-- EPITECH PROJECT, 2025
-- control flow
-- File description:
-- hs
-}

{-# LANGUAGE NamedFieldPuns #-}

module ControlFlowInstr (
    controlFlowReturn,
    controlFlowReturnInt,
    controlFlowGoto,
    controlFlowInvokeStatic,
    controlFlowInvokeVirtual,
    controlFlowInvokeSpecial
) where

import Data
import qualified Data.Map as Map
import qualified Data.Vector as V


controlFlowReturn :: VMState -> Either String VMState
controlFlowReturn st@VMState{functions, currentFunc, frames} =
    case frames of
        [] ->
            case Map.lookup currentFunc functions of
                Nothing -> Left ("Function not found: " ++ currentFunc)
                Just func -> Right st {ip = V.length (funcCode func)}
        (Frame{fIP, fFunction}:restFrames) ->
            Right st { 
                ip = fIP,
                currentFunc = fFunction,
                frames = restFrames
            }


controlFlowReturnInt :: VMState -> Either String VMState
controlFlowReturnInt st@VMState{stack, functions, currentFunc, frames} =
    case stack of
        [] -> Left "Stack underflow in IReturnInt"
        (VInt returnValue:restStack) ->
            case frames of
                [] ->
                    case Map.lookup currentFunc functions of
                        Nothing -> Left ("Function not found: " ++ currentFunc)
                        Just func -> Right st { 
                            ip = V.length (funcCode func),
                            stack = [VInt returnValue]
                        }
                (Frame{fIP, fFunction}:restFrames) ->
                    Right st {
                        ip = fIP,
                        currentFunc = fFunction,
                        frames = restFrames,
                        stack = VInt returnValue : restStack
                    }
        _ -> Left "IReturnInt expects an integer on top of the stack"


controlFlowGoto :: Int -> VMState -> Either String VMState
controlFlowGoto offset st =
    Right st {ip = offset}


controlFlowInvokeStatic :: String -> VMState -> Either String VMState
controlFlowInvokeStatic funcName st@VMState{functions, frames, currentFunc, ip} =
    case Map.lookup funcName functions of
        Nothing -> Left ("Function not found: " ++ funcName)
        Just _ -> 
            let newFrame = Frame {fLocals = [], fIP = ip + 1, fFunction = currentFunc}
            in Right st { 
                ip = 0,
                currentFunc = funcName,
                frames = newFrame : frames
            }


controlFlowInvokeVirtual :: String -> VMState -> Either String VMState
controlFlowInvokeVirtual = controlFlowInvokeStatic


controlFlowInvokeSpecial :: String -> VMState -> Either String VMState
controlFlowInvokeSpecial = controlFlowInvokeStatic
