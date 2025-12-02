{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Main
-}

module Main (main) where

import Shell

main :: IO ()
main = (writeHelp >> initializeEnv) >>= \env -> 
  startShell env
