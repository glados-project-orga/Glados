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

tests :: Test
tests = TestList[]

main :: IO Counts
main = runTestTT tests
