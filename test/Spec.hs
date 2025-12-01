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
import LispBuiltinsTest

tests :: Test
tests = TestList[
    testBuiltins
    ]

main :: IO Counts
main = runTestTT tests
