{- 
-- EPITECH PROJECT, 2025
-- vm
-- File description:
-- IOInstr.hs - IO instructions (invoke_write)
-}

{-# LANGUAGE NamedFieldPuns #-}

module IOInstr (
    invokeWrite
) where

import Data
import System.IO (hPutStr, stdout, stderr)

invokeWrite :: Int -> VMState -> IO (Either String VMState)
invokeWrite fd st@VMState{stack, ip} =
    case stack of
        [] -> pure $ Left "invoke_write: empty stack"
        (val:rest) ->
            case fd of
                1 -> hPutStr stdout output >>= \_ -> pure $ Right st {ip = ip + 1, stack = rest}
                2 -> hPutStr stderr output >>= \_ -> pure $ Right st {ip = ip + 1, stack = rest}
                _ -> pure $ Right st {ip = ip + 1, stack = rest}
            where output = valueToString val

valueToString :: Value -> String
valueToString (VInt n) = show n
valueToString (VBool False) = "false"
valueToString (VBool True) = "true"
valueToString (VChar c) = [c]
valueToString (VFloat f) = show f
valueToString (VDouble d) = show d
valueToString (VLong l) = show l
valueToString VNull = "null"
valueToString (VHandle h) = "<handle:" ++ show h ++ ">"
