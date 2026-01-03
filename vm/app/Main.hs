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
    Left err -> print ("Error: " ++ err) >> exitWith (ExitFailure 84)
    Right instrs -> print instrs
