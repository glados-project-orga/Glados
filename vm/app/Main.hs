{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- Main entry point
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Loader (loadBytecode)
import Data (VMState(..), Value(..), Function(..))
import Vmstate (compile)
import qualified Data.Map as Map
import qualified Data.Vector as V

main :: IO ()
main = getArgs >>= \args ->
  case args of
    [file] -> runFile file
    _      -> usage >> exitWith (ExitFailure 84)

usage :: IO ()
usage = hPutStrLn stderr "Usage: glados-vm <bytecode-file>"

runFile :: FilePath -> IO ()
runFile path = readFile path >>= \content ->
  case loadBytecode content of
    Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitWith (ExitFailure 84)
    Right parsedFuncs -> runVM parsedFuncs

runVM :: Map.Map String Function -> IO()
runVM parsedFuncs = 
  case Map.lookup "main" parsedFuncs of
    Nothing -> hPutStrLn stderr "Error: No 'main' function found" >> exitWith (ExitFailure 84)
    Just _ -> 
      let initialState = VMState
            { stack      = []
            , locals     = V.replicate 10 (VInt 0)
            , ip         = 0
            , functions  = parsedFuncs
            , currentFunc = "main"
            , constPool  = V.empty
            , heap       = V.empty
            , frames     = []
            }
      in compile initialState >>= \result ->
        case result of
          Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitWith (ExitFailure 84)
          Right finalState -> handleResult (stack finalState)

handleResult :: [Value] -> IO ()
handleResult [] = return ()
handleResult (VInt 0:_) = exitWith ExitSuccess
handleResult (VInt code:_) = exitWith (ExitFailure code)
handleResult _ = return ()