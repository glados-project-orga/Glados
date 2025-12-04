{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- LispBuiltinsTest.hs
-}

module LispBuiltinsTest (
    testBuiltins
) where

import Test.HUnit
import Builtins(
    foundInt,
    foundBool,
    builtinAdd,
    builtinSub,
    builtinMul,
    builtinMod, 
    builtinDiv,
    builtinEqual,
    builtinLower,
    builtinGreater,
    builtinIf
    )
import Data(Ast(..))

testFoundInt :: Test
testFoundInt = TestList[
    TestCase(assertEqual "foundInt AInt 5" (Right 5) (foundInt (AInt 5))),
    TestCase(assertEqual "foundInt ASymbol foo" (Left "foo is not a number") (foundInt (ASymbol "foo")))
    ]

testFoundBool :: Test
testFoundBool = TestList[
    TestCase(assertEqual "foundBool ABool True" (Right True) (foundBool (ABool True))),
    TestCase(assertEqual "foundBool ASymbol foo" (Left "foo is not a boolean") (foundBool (ASymbol "foo")))
    ]

testBuiltinsAdd :: Test
testBuiltinsAdd = TestList[
    TestCase(assertEqual "(+)" (Right (AInt 0)) (builtinAdd [])),
    TestCase(assertEqual "(+ 67)" (Right (AInt 67)) (builtinAdd [AInt 67])),
    TestCase(assertEqual "(+ 2 2)" (Right (AInt 4)) (builtinAdd [AInt 2, AInt 2])),
    TestCase(assertEqual "(+ 1 2 3)" (Right (AInt 6)) (builtinAdd [AInt 1, AInt 2, AInt 3]))
    ]

testBuiltinsSub :: Test
testBuiltinsSub = TestList[
    TestCase(assertEqual "(-)" (Left "Exception: incorrect argument count in call (-)") (builtinSub [])),
    TestCase(assertEqual "(- 4)" (Right (AInt (-4))) (builtinSub [AInt 4])),
    TestCase(assertEqual "(- 2 1)" (Right (AInt 1)) (builtinSub [AInt 2, AInt 1])),
    TestCase(assertEqual "(- 6 2 1)" (Right (AInt 3)) (builtinSub [AInt 6, AInt 2, AInt 1]))
    ]

testBuiltinsMul :: Test
testBuiltinsMul = TestList[
    TestCase(assertEqual "(*)" (Right (AInt 1)) (builtinMul [])),
    TestCase(assertEqual "(* 4)" (Right (AInt 4)) (builtinMul [AInt 4])),
    TestCase(assertEqual "(* 2 2)" (Right (AInt 4)) (builtinMul [AInt 2, AInt 2])),
    TestCase(assertEqual "(* 2 2 3)" (Right (AInt 12)) (builtinMul [AInt 2, AInt 2, AInt 3]))
    ]

testBuiltinsDiv :: Test
testBuiltinsDiv = TestList[
    TestCase(assertEqual "(div 1)" (Left "Exception: incorrect argument count in call (div 1)") (builtinDiv [AInt 1])),
    TestCase(assertEqual "(div 2 2)" (Right (AInt 1)) (builtinDiv [AInt 2, AInt 2])),
    TestCase(assertEqual "(div 2 0)" (Left "Exception in div: undefined for 0") (builtinDiv [AInt 2, AInt 0]))
    ]

testBuiltinsMod :: Test
testBuiltinsMod = TestList[
    TestCase(assertEqual "(mod 1)" (Left "Exception: incorrect argument count in call (mod 1)") (builtinMod [AInt 1])),
    TestCase(assertEqual "(mod 5 2)" (Right (AInt 1)) (builtinMod [AInt 5, AInt 2])),
    TestCase(assertEqual "(mod 2 0)" (Left "Exception in mod: undefined for 0") (builtinMod [AInt 2, AInt 0]))
    ]

testBuiltinsEqual :: Test
testBuiltinsEqual = TestList[
    TestCase(assertEqual "(eq?)" (Left "Exception: incorrect argument count in call (eq?)") (builtinEqual [])),
    TestCase(assertEqual "(eq? 1 2)" (Right (ABool False)) (builtinEqual [AInt 1, AInt 2])),
    TestCase(assertEqual "(eq? 1 1)" (Right (ABool True)) (builtinEqual [AInt 1, AInt 1]))
    ]

testBuiltinsLower :: Test
testBuiltinsLower = TestList[
    TestCase(assertEqual "(<)" (Left "Exception: incorrect argument count in call (<)") (builtinLower [])),
    TestCase(assertEqual "(< 2 1)" (Right (ABool False)) (builtinLower [AInt 2, AInt 1])),
    TestCase(assertEqual "(< 1 2 3)" (Right (ABool True)) (builtinLower [AInt 1, AInt 2, AInt 3])),
    TestCase(assertEqual "(< 1 2 2)" (Right (ABool False)) (builtinLower [AInt 1, AInt 2, AInt 2]))
    ]

testBuiltinsGreater :: Test
testBuiltinsGreater = TestList[
    TestCase(assertEqual "(>)" (Left "Exception: incorrect argument count in call (>)") (builtinGreater [])),
    TestCase(assertEqual "(> 1 2)" (Right (ABool False)) (builtinGreater [AInt 1, AInt 2])),
    TestCase(assertEqual "(> 3 2 1)" (Right (ABool True)) (builtinGreater [AInt 3, AInt 2, AInt 1])),
    TestCase(assertEqual "(> 2 2 1)" (Right (ABool False)) (builtinGreater [AInt 2, AInt 2, AInt 1]))
    ]

testBuiltinsIf :: Test
testBuiltinsIf = TestList[
    TestCase(assertEqual "(if)" (Left "Exception: invalid syntax (if)") (builtinIf [])),
    TestCase(assertEqual "(if #t 1)" (Right (AInt 1)) (builtinIf [ABool True, AInt 1])),
    TestCase(assertEqual "(if #f 1)" (Right AVoid) (builtinIf [ABool False, AInt 1])),
    TestCase(assertEqual "(if #t 2 1)" (Right (AInt 2)) (builtinIf [ABool True, AInt 2, AInt 1])),
    TestCase(assertEqual "(if #f 2 1)" (Right (AInt 1)) (builtinIf [ABool False, AInt 2, AInt 1]))
    ]

testBuiltins :: Test
testBuiltins = TestList[
    testFoundInt,
    testFoundBool,
    testBuiltinsAdd,
    testBuiltinsSub,
    testBuiltinsMul,
    testBuiltinsDiv,
    testBuiltinsMod,
    testBuiltinsEqual,
    testBuiltinsLower,
    testBuiltinsGreater,
    testBuiltinsIf
    ]
