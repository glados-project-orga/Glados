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
import SymTableTest (testSymTable)
import CompilerToolsTest (testCompilerTools)
import NormalizeTests (testNormalize)

tests :: Test
tests = TestList[
    testSymTable,
    testCompilerTools,
    testNormalize
    ]

main :: IO Counts
main = runTestTT tests
