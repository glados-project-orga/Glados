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
import ParserTest
import EvalTest
import DataTest
import RuntimeTest

tests :: Test
tests = TestList[
    testBuiltins,
    testSExpr,
    testParser,
    testEval,
    testData,
    testRuntime
    ]

main :: IO Counts
main = runTestTT tests
