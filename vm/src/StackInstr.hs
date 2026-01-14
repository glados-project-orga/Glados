{- 
-- EPITECH PROJECT, 2025
-- Stack
-- File description:
-- instr
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module StackInstr (
      stackInstrLoadInt,
      stackInstrStoreInt,
      stackInstrSwap,
      stackInstrPop,
      stackInstrDup,
      stackInstrConstInt,
      stackInstrLdc,
      stackInstrINop,
      stackInstrIinc,
      stack_All_Instr,
      stack_chargement
) where


import Data
import Data.Int (Int64)
import qualified Data.Vector as V


stackInstrConstInt :: Int -> VMState -> Either String VMState
stackInstrConstInt n st@VMState{stack, ip} =
    Right st {ip = ip + 1, stack = VInt n : stack}

stackInstrConstFloat :: Float -> VMState -> Either String VMState
stackInstrConstFloat f st@VMState{stack, ip} =
    Right st {ip = ip + 1, stack = VFloat f : stack}

stackInstrConstDouble :: Double -> VMState -> Either String VMState
stackInstrConstDouble d st@VMState{stack, ip} =
    Right st {ip = ip + 1, stack = VDouble d : stack}

stackInstrConstLong :: Int64 -> VMState -> Either String VMState
stackInstrConstLong l st@VMState{stack, ip} =
    Right st {ip = ip + 1, stack = VLong l : stack}

stackInstrConstChar :: Char -> VMState -> Either String VMState
stackInstrConstChar c st@VMState{stack, ip} =
    Right st {ip = ip + 1, stack = VChar c : stack}

stackInstrLdc :: Int -> VMState -> Either String VMState
stackInstrLdc n st@VMState{stack, ip, constPool} =
    case constPool V.!? n of
        Just val -> Right st { ip = ip + 1, stack = val : stack }
        Nothing  -> Left ("Invalid constant pool index: " ++ show n)


stackInstrLoadInt :: Int -> VMState -> Either String VMState
stackInstrLoadInt n st@VMState{stack, ip, locals} =
    case locals V.!? n of
        Just (VInt val) -> Right st {ip = (ip + 1), stack = (VInt val : stack)}
        Just _ -> Left ("iload: expected int at local index " ++ show n) 
        Nothing  -> Left ("Invalid local index: " ++ show n)


stackInstrLoadFloat :: Int -> VMState -> Either String VMState
stackInstrLoadFloat n st@VMState{stack, ip, locals} =
    case locals V.!? n of
        Just (VFloat f) -> Right st {ip = (ip + 1), stack = (VFloat f: stack)}
        Just _ -> Left ("fload: expected float at local index " ++ show n) 
        Nothing -> Left ("Invalid local index: " ++ show n)


stackInstrLoadLong :: Int -> VMState -> Either String VMState
stackInstrLoadLong n st@VMState{stack, ip, locals} =
    case locals V.!? n of
        Just (VLong l) -> Right st {ip = (ip + 1), stack = (VLong l: stack)}
        Just _ -> Left ("lload: expected long at local index " ++ show n) 
        Nothing -> Left ("Invalid local index: " ++ show n)


stackInstrALoad :: Int -> VMState -> Either String VMState
stackInstrALoad n st@VMState{stack, ip, locals} =
    case locals V.!? n of
        Just (VInt v) -> Right st { ip = ip + 1 , stack = (VInt v : stack)}
        Just _ -> Left ("aload: expected reference at local index " ++ show n)
        Nothing -> Left ("Invalid local index: " ++ show n)


stackInstrAStore :: Int -> VMState -> Either String VMState
stackInstrAStore n st@VMState{stack, ip, locals} =
    case stack of
        (VInt v : rest) ->
            Right st { ip = ip + 1, stack = rest, locals = (locals V.// [(n, VInt v)])}
        (_ : _) ->
            Left ("astore: expected reference on stack for local index " ++ show n)
        [] -> Left "astore: empty stack"


stackInstrStoreInt :: Int -> VMState -> Either String VMState
stackInstrStoreInt n st@VMState{stack, ip, locals} =
    case stack of
        (VInt val : rest) ->
            let newLocals = locals V.// [(n, VInt val)]
            in Right st {ip = (ip + 1), stack = rest, locals = newLocals}
        _ -> Left ("istore: expected int on stack for local index " ++ show n)


stackInstrStoreFloat :: Int -> VMState -> Either String VMState
stackInstrStoreFloat n st@VMState{stack, ip, locals} =
    case stack of
        (VFloat f: rest) ->
            let newlocals = locals V.//[(n, VFloat f)] 
            in Right st {ip = (ip + 1), stack = rest, locals = newlocals}
        _ -> Left ("fstore: expected float on stack for local index " ++ show n)


stackInstrStoreLong :: Int -> VMState -> Either String VMState
stackInstrStoreLong n st@VMState{stack, ip, locals} =
    case stack of
        (VLong l: rest) ->
            let newlocals = locals V.//[(n, VLong l)] 
            in Right st {ip = (ip + 1), stack = rest, locals = newlocals}
        _ -> Left ("lstore: expected long on stack for local index " ++ show n)


stackInstrLoadDouble :: Int -> VMState -> Either String VMState
stackInstrLoadDouble n st@VMState{stack, ip, locals} =
    case locals V.!? n of
        Just (VDouble d) -> Right st {ip = ip + 1, stack = VDouble d : stack}
        Just _ -> Left ("dload: expected double at local index " ++ show n) 
        Nothing -> Left ("Invalid local index: " ++ show n)


stackInstrStoreDouble :: Int -> VMState -> Either String VMState
stackInstrStoreDouble n st@VMState{stack, ip, locals} =
    case stack of
        (VDouble d : rest) ->
            let newlocals = locals V.// [(n, VDouble d)] 
            in Right st {ip = ip + 1, stack = rest, locals = newlocals}
        _ -> Left ("dstore: expected double on stack for local index " ++ show n)


stackInstrLoadChar :: Int -> VMState -> Either String VMState
stackInstrLoadChar n st@VMState{stack, ip, locals} =
    case locals V.!? n of
        Just (VChar c) -> Right st {ip = ip + 1, stack = VChar c : stack}
        Just _ -> Left ("cload: expected char at local index " ++ show n) 
        Nothing -> Left ("Invalid local index: " ++ show n)


stackInstrStoreChar :: Int -> VMState -> Either String VMState
stackInstrStoreChar n st@VMState{stack, ip, locals} =
    case stack of
        (VChar c : rest) ->
            let newlocals = locals V.// [(n, VChar c)] 
            in Right st {ip = ip + 1, stack = rest, locals = newlocals}
        _ -> Left ("cstore: expected char on stack for local index " ++ show n)


stackInstrSwap :: VMState -> Either String VMState
stackInstrSwap st@VMState{stack, ip} =
    case stack of
        (v1:v2:rest) ->
            Right st {ip = (ip + 1), stack = (v2:v1:rest)}
        _ -> Left "ISwap expects at least two values"


stackInstrPop :: Int -> VMState -> Either String VMState
stackInstrPop n st@VMState{stack, ip} =
    if length stack < n
        then Left ("IPop" ++ show n ++ " expects at least " ++ show n ++ " values")
        else Right st {ip = (ip + 1), stack = (drop n stack)}


stackInstrDup :: WhatDup -> VMState -> Either String VMState
stackInstrDup dup st@VMState{stack, ip} =
    case (dup, stack) of

        (Dup, (v1:rest)) ->
            Right st {ip = (ip + 1), stack = (v1:v1:rest)}

        (Dup2, (v2:v1:rest)) ->
            Right st {ip = (ip + 1), stack = (v2:v1:v2:v1:rest)}

        (DupX1, (v1:v2:rest)) ->
            Right st {ip = (ip + 1), stack = (v1:v2:v1:rest)}

        (DupX2, (v1:v2:v3:rest)) ->
            Right st {ip = (ip + 1), stack = (v1:v3:v2:v1:rest)}

        (Dup2X1, (v1:v2:v3:rest)) ->
            Right st {ip = (ip + 1), stack = (v2:v1:v3:v2:v1:rest)}

        (Dup2X2, (v1:v2:v3:v4:rest)) ->
            Right st {ip = (ip + 1), stack = (v2:v1:v4:v3:v2:v1:rest)}

        _ -> Left "Invalid stack shape for DUP instruction"


stackInstrINop :: VMState -> Either String VMState
stackInstrINop st@VMState{ip} = Right st {ip = ip + 1}


stack_All_Instr :: StackIns -> VMState -> Either String VMState
stack_All_Instr ins st =
    case ins of
        IPop      -> stackInstrPop 1 st
        IPop2     -> stackInstrPop 2 st

        IDup      -> stackInstrDup Dup st
        IDupX1    -> stackInstrDup DupX1 st
        IDupX2    -> stackInstrDup DupX2 st
        IDup2     -> stackInstrDup Dup2 st
        IDup2X1   -> stackInstrDup Dup2X1 st
        IDup2X2   -> stackInstrDup Dup2X2 st

        ISwap     -> stackInstrSwap st
        INop      -> stackInstrINop st


stack_chargement :: StackCharg -> VMState -> Either String VMState
stack_chargement ins st =
    case ins of
        AStore n        -> stackInstrAStore n st
        ALoad  n        -> stackInstrALoad n st

        ILoadInt n      -> stackInstrLoadInt n st
        IStoreInt n     -> stackInstrStoreInt n st
        IConstInt n     -> stackInstrConstInt n st

        ILoadFloat n    -> stackInstrLoadFloat n st
        IStoreFloat n   -> stackInstrStoreFloat n st
        IConstFloat f   -> stackInstrConstFloat f st
    
        ILoadLong n     -> stackInstrLoadLong n st
        IStoreLong n    -> stackInstrStoreLong n st
        IConstLong l    -> stackInstrConstLong l st

        ILoadDouble n   -> stackInstrLoadDouble n st
        IStoreDouble n  -> stackInstrStoreDouble n st
        IConstDouble d  -> stackInstrConstDouble d st

        ILoadChar n     -> stackInstrLoadChar n st
        IStoreChar n    -> stackInstrStoreChar n st
        IConstChar c    -> stackInstrConstChar c st


stackInstrIinc :: Int -> Int -> VMState -> Either String VMState
stackInstrIinc idx inc st@VMState{ip, locals} =
    case locals V.!? idx of
        Just (VInt val) ->
            let newLocals = locals V.// [(idx, VInt (val + inc))]
            in Right st {ip = ip + 1, locals = newLocals}
        Just _ -> Left ("iinc: expected int at local index " ++ show idx)
        Nothing -> Left ("iinc: invalid local index " ++ show idx)