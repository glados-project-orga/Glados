{-
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- Spec.hs
-}

module Main (
    main
) where

import Test.HUnit
import CompilerTest (testCompiler)

tests :: Test
tests = TestList[
    testCompiler
    ]

main :: IO Counts
main = runTestTT tests
