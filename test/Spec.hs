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
import Builtins(foundInt, builtinAdd, builtinSub, builtinMul, builtinMod, builtinDiv)
import Data(Ast(..))

testFoundInt :: Test
testFoundInt = TestList[
    TestCase(assertEqual "foundInt AInt 5" (Right 5) (foundInt (AInt 5))),
    TestCase(assertEqual "foundInt ASymbol foo" (Left "foo is not a number") (foundInt (ASymbol "foo")))
    ]

testBuiltinsAdd :: Test
testBuiltinsAdd = TestList[
    TestCase(assertEqual "(+)" (Right (AInt 0)) (builtinAdd [])),
    TestCase(assertEqual "(+ 2 2)" (Right (AInt 4)) (builtinAdd [AInt 2, AInt 2])),
    TestCase(assertEqual "(+ 1 2 3)" (Right (AInt 6)) (builtinAdd [AInt 1, AInt 2, AInt 3]))
    ]

testBuiltinsSub :: Test
testBuiltinsSub = TestList[
    TestCase(assertEqual "(-)" (Left "Exception: incorrect argument count in call (-)") (builtinSub [])),
    TestCase(assertEqual "(- 2 1)" (Right (AInt 1)) (builtinSub [AInt 2, AInt 1])),
    TestCase(assertEqual "(- 6 2 1)" (Right (AInt 3)) (builtinSub [AInt 6, AInt 2, AInt 1]))
    ]

testBuiltinsMul :: Test
testBuiltinsMul = TestList[
    TestCase(assertEqual "(*)" (Right (AInt 1)) (builtinMul [])),
    TestCase(assertEqual "(* 2 2)" (Right (AInt 4)) (builtinMul [AInt 2, AInt 2])),
    TestCase(assertEqual "(* 2 2 3)" (Right (AInt 12)) (builtinMul [AInt 2, AInt 2, AInt 3]))
    ]

testBuiltinsDiv :: Test
testBuiltinsDiv = TestList[
    TestCase(assertEqual "(div 1)" (Left "Exception: incorrect argument count in call (div 1)") (builtinDiv [AInt 1])),
    TestCase(assertEqual "(div 2 2)" (Right (AInt 1)) (builtinDiv [AInt 2, AInt 2]))
    ]

testBuiltinsMod :: Test
testBuiltinsMod = TestList[
    TestCase(assertEqual "(mod 1)" (Left "Exception: incorrect argument count in call (mod 1)") (builtinMod [AInt 1])),
    TestCase(assertEqual "(mod 5 2)" (Right (AInt 1)) (builtinMod [AInt 5, AInt 2]))
    ]

testBuiltins :: Test
testBuiltins = TestList[
    testFoundInt,
    testBuiltinsAdd,
    testBuiltinsSub,
    testBuiltinsMul,
    testBuiltinsDiv,
    testBuiltinsMod
    ]

tests :: Test
tests = TestList[
    testBuiltins
    ]

main :: IO Counts
main = runTestTT tests
