{- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Runtime tests (tests d'integration)
-}

module RuntimeTest (
    testRuntime
) where

import Test.HUnit
import System.Process (readProcessWithExitCode)
import System.Exit    (ExitCode(..))

testRuntime :: Test
testRuntime = TestList [
    testValidProgram,
    testErrorProgram
    ]

testValidProgram :: Test
testValidProgram = TestCase (
    readProcessWithExitCode "./glados" ["tests/valid.lisp"] "" >>=
        (\(code, _, _) -> assertEqual
            "Program must exit 0 on valid input"
            ExitSuccess
            code)
    )

testErrorProgram :: Test
testErrorProgram = TestCase (
    readProcessWithExitCode "./glados" ["tests/error.lisp"] "" >>=
        (\(code, _, _) -> assertEqual
            "Program must exit 84 on error input"
            (ExitFailure 84)
            code)
    )
