{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- Test entry point
-}

module Main (
  main
) where

import Test.HUnit
import VmTest(testVMConds)
import VmTest1(testHeapInstr)
import VmTest2(testControlFlow)

tests :: Test
tests = TestList[
    testVMConds,
    testHeapInstr,
    testControlFlow
  ]

main :: IO Counts
main = runTestTT tests