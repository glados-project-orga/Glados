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
import SExprToAstTest

tests :: Test
tests = TestList[
    testBuiltins,
    testSExpr
    ]

main :: IO Counts
main = runTestTT tests
