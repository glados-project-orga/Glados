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
import Builtins(apply)
import Data(Ast(..))

testBuiltinsAdd :: Test
testBuiltinsAdd = TestList[
    TestCase(assertEqual "2 + 2" (Just (AInt 4)) (apply "+" [AInt 2, AInt 2])),
    TestCase(assertEqual "1 + 2 + 3" (Just (AInt 6)) (apply "+" [AInt 1, AInt 2, AInt 3]))
    ]

testBuiltinsSub :: Test
testBuiltinsSub = TestList[
    TestCase(assertEqual "2 - 1" (Just (AInt 1)) (apply "-" [AInt 2, AInt 1])),
    TestCase(assertEqual "6 - 2 - 1" (Just (AInt 3)) (apply "-" [AInt 6, AInt 2, AInt 1]))
    ]

testBuiltinsMul :: Test
testBuiltinsMul = TestList[
    TestCase(assertEqual "2 * 2" (Just (AInt 4)) (apply "*" [AInt 2, AInt 2])),
    TestCase(assertEqual "2 * 2 * 3" (Just (AInt 12)) (apply "*" [AInt 2, AInt 2, AInt 3]))
    ]


tests :: Test
tests = TestList[
    testBuiltinsAdd,
    testBuiltinsSub,
    testBuiltinsMul
    ]

main :: IO Counts
main = runTestTT tests
