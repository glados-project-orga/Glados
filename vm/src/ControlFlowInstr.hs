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
    controlFlowGoto_w,
    controlFlowReturnDouble,
    controlFlowReturnFloat,
    controlFlowReturnLong,
    controlFlowReturnA,
    controlFlowInvokeStatic
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


controlFlowReturnA :: VMState -> Either String VMState
controlFlowReturnA st@VMState{stack, functions, currentFunc, frames} =
    case stack of
        [] -> Left "Stack underflow in AReturn"
        ((VHandle v) : restStack) ->
            case frames of
                [] ->
                    case Map.lookup currentFunc functions of
                        Nothing -> Left ("Function not found: " ++ currentFunc)
                        Just func -> Right st { 
                            ip = V.length (funcCode func),
                            stack = [VHandle v]
                        }
                (Frame{fIP, fFunction}:restFrames) ->
                    Right st {
                        ip = fIP,
                        currentFunc = fFunction,
                        frames = restFrames,
                        stack = VHandle v: restStack
                    }
        _ -> Left "AReturn: expected a reference (VHandle) on the stack"


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



controlFlowReturnFloat :: VMState -> Either String VMState
controlFlowReturnFloat st@VMState{stack, functions, currentFunc, frames} =
    case stack of
        [] -> Left "Stack underflow in IReturnFloat"
        (VFloat returnValue:restStack) ->
            case frames of
                [] ->
                    case Map.lookup currentFunc functions of
                        Nothing -> Left ("Function not found: " ++ currentFunc)
                        Just func -> Right st { 
                            ip = V.length (funcCode func),
                            stack = [VFloat returnValue]
                        }
                (Frame{fIP, fFunction}:restFrames) ->
                    Right st {
                        ip = fIP,
                        currentFunc = fFunction,
                        frames = restFrames,
                        stack = VFloat returnValue : restStack
                    }
        _ -> Left "IReturnFloat expects an float on top of the stack"


controlFlowReturnDouble :: VMState -> Either String VMState
controlFlowReturnDouble st@VMState{stack, functions, currentFunc, frames} =
    case stack of
        [] -> Left "Stack underflow in IReturnDouble"
        (VDouble returnValue:restStack) ->
            case frames of
                [] ->
                    case Map.lookup currentFunc functions of
                        Nothing -> Left ("Function not found: " ++ currentFunc)
                        Just func -> Right st { 
                            ip = V.length (funcCode func),
                            stack = [VDouble returnValue]
                        }
                (Frame{fIP, fFunction}:restFrames) ->
                    Right st {
                        ip = fIP,
                        currentFunc = fFunction,
                        frames = restFrames,
                        stack = VDouble returnValue : restStack
                    }
        _ -> Left "IReturnIDouble expects an double on top of the stack"


controlFlowReturnLong :: VMState -> Either String VMState
controlFlowReturnLong st@VMState{stack, functions, currentFunc, frames} =
    case stack of
        [] -> Left "Stack underflow in IReturnLong"
        (VLong returnValue:restStack) ->
            case frames of
                [] ->
                    case Map.lookup currentFunc functions of
                        Nothing -> Left ("Function not found: " ++ currentFunc)
                        Just func -> Right st { 
                            ip = V.length (funcCode func),
                            stack = [VLong returnValue]
                        }
                (Frame{fIP, fFunction}:restFrames) ->
                    Right st {
                        ip = fIP,
                        currentFunc = fFunction,
                        frames = restFrames,
                        stack = VLong returnValue : restStack
                    }
        _ -> Left "IReturnLong expects an long on top of the stack"



controlFlowGoto :: Int -> VMState -> Either String VMState
controlFlowGoto offset st =
    Right st {ip = offset}


controlFlowGoto_w :: Int -> VMState -> Either String VMState
controlFlowGoto_w offset st =
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
