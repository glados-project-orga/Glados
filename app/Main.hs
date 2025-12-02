{-
-- EPITECH PROJECT, 2025
-- gladeeeosss
-- File description:
-- Main
-}

module Main (main) where

import Shell
import Parser
import Eval(evalAST)
import qualified Data.Map as Map

main :: IO ()
main = (writeHelp >> initializeEnv) >>= \env -> 
  startShell env
