{-
-- EPITECH PROJECT, 2025
-- glados-repo
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import Args (checkArgs)

main :: IO ()
main = getArgs >>= \args ->
    checkArgs args >>= either (putStrLn . ("Arguments Error: " ++)) (\content -> 
        putStrLn $ "File content loaded successfully:\n" ++ content)