{- 
-- EPITECH PROJECT, 2025
-- boostrap
-- File description:
-- part2.hs
-}

import Data2

import qualified Data.Map as Map

applyOp :: Op -> Stack -> Either String Stack

applyOp Add (VInt x: VInt xs : ys) = Right(VInt(xs + x) : ys)
applyOp Sub (VInt x: VInt xs : ys) = Right(VInt(xs - x) : ys)
applyOp Mul (VInt x: VInt xs : ys) = Right(VInt(xs * x) : ys)

applyOp  Div (VInt x: VInt xs : ys) =
    if x == 0 then Left ("Error : division by 0")
    else  Right (VInt(xs `div` x) : ys)

applyOp Eq (VInt x: VInt xs : ys) =  Right(VBool (xs == x) : ys)
applyOp Less (VInt x: VInt xs : ys) = Right(VBool (xs > x) : ys)

applyOp _ [_] = Left "Error: operation needs two arguments"
applyOp _ [] = Left "Error: empty stack"
applyOp _ _ =  Left "Invalid operands for operation"
