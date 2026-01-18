{- 
-- EPITECH PROJECT, 2025
-- vm
-- File description:
-- IOInstr.hs - IO instructions (invoke_write)
-}

{-# LANGUAGE NamedFieldPuns #-}

module IOInstr (
    invokeWrite,
    invokeOpen
) where

import Data
import Control.Exception (try, IOException)
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
valueToString (VString s) = s
valueToString VNull = "null"
valueToString (VHandle h) = "<handle:" ++ show h ++ ">"


invokeOpen :: VMState -> IO (Either String VMState)
invokeOpen st@VMState{stack, ip} =
    case stack of
        [] -> pure $ Left "invoke_read: empty stack"
        (VString file : rest) ->
            readFileforMe file >>= \result ->
                case result of
                    Left err -> pure $ (Left ("invoke_read: " ++ err))
                    Right content -> pure $ (Right st {stack = (VString content : rest) , ip = (ip + 1)})
        (x : _) -> pure $ Left ("invoke_read: expected string, got " ++ show x) 


readFileforMe :: String -> IO (Either String String)
readFileforMe path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left err      -> pure (Left (show err))
        Right content -> pure (Right content)
