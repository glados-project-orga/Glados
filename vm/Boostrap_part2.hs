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

applyOp Less (VInt x: VInt xs : ys) = Right(VBool (xs < x) : ys)

applyOp _ [_] = Left "Error: operation needs two arguments"

applyOp _ [] = Left "Error: empty stack"

applyOp _ _ =  Left "Invalid operands for operation"



exec :: Env -> Args -> Insts -> Stack -> Either String Value

exec    _     _       []        (v: _)  = Right v
exec    env   args   (IPush v:xs) st    = exec env args xs (v : st)
exec    _     _      (IRet : _)  (v:_)  = Right v

exec    env  args (IJumpIfFalse v: xs) (VBool b: ys) =
    if b then exec env args xs ys
    else
        if v <= length xs then  exec env args(drop v xs) ys 
        else Left "Error: jump out of bounds"  


exec   env   args (IPushArg i : xs) st  =
    case drop i args of 
        (v:_) -> exec env args xs (v : st) 
        [] -> Left "Error: argument index out of bounds"


exec   env   args (ICall: xs) (VOp op: ys) =
    case (applyOp op ys) of
        Left err -> Left err
        Right stt -> exec env args xs stt


exec env args (ICall : xs) (VFun body : arg : st) =
    case exec env [arg] body [] of
        Left err -> Left err
        Right v  -> exec env args xs (v : st)


exec  env  args (IPushEnv sttr : xs) stackk =
    case Map.lookup sttr env of
       Just body -> exec env args xs (VFun body : stackk)
       Nothing  -> Left "Error: Unknow function"

exec _ _ (ICall : _) _ = Left "Error: trying to call a non-callable value" 
exec _ _  _ _ = Left "Error: Invalid state"
