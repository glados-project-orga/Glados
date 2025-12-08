{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Main
-}

module Main (main) where

import Shell
import System.IO (hIsTerminalDevice, stdin)

main :: IO ()
main = (writeHelp >> initializeEnv) >>= \env -> 
  hIsTerminalDevice stdin >>= \redirection ->
    case redirection of
      True -> startShell env
      False -> readAll "" >>= \content -> evalRedirShell content env

