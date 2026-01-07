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
    controlFlowGoto
) where

import Data
import qualified Data.Vector as V


controlFlowReturn :: VMState -> Either String VMState
controlFlowReturn st@VMState{code} =
    Right st { ip = V.length code }


controlFlowReturnInt :: VMState -> Either String VMState
controlFlowReturnInt st@VMState{stack, code} =
    case stack of
        [] -> Left "Stack underflow in IReturnInt"
        (VInt _:_) -> Right st { ip = V.length code }
        _ -> Left "IReturnInt expects an integer on top of the stack"


controlFlowGoto :: Int -> VMState -> Either String VMState
controlFlowGoto offset st =
    Right st { ip = offset }
