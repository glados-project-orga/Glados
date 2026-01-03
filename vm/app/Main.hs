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

main :: IO ()
main = getArgs >>= \args ->
  case args of
    [file] -> runFile file
    _      -> usage >> exitWith (ExitFailure 84)

usage :: IO ()
usage = hPutStrLn stderr "Usage: glados-vm <bytecode-file>"

runFile :: FilePath -> IO ()
runFile path = readFile path >>= \content ->
  putStrLn content
