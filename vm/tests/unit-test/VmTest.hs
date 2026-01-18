{-
-- EPITECH PROJECT, 2025
-- unit test
-- File description:
-- VMCondTest.hs
-}

module VmTest (
    testVMConds
) where

import Test.HUnit
import Data.Vector as V
import Data.Map as Map
import Data (VMState(..), Value(..))
import ComparInstr (
    compIfNe,
    compIfLt,
    compIfGe,
    compIfLe,
    compIfACmpEq,
    compIfACmpNe,
    compIfICmpGt,
    compIfICmpEq,
    compIfICmpNe,
    compIfICmpGe,
    compIfICmpLe,
    compLCmp,
    compFCmpL,
    compDCmpG
    )


mkState :: [Value] -> Int -> VMState
mkState stk i = VMState
    { stack       = stk
    , locals      = V.empty
    , ip          = i
    , functions   = Map.empty
    , currentFunc = ""
    , constPool   = V.empty
    , heap        = V.empty
    , frames      = []
    }

testIfNe :: Test
testIfNe = TestList [
    TestCase(assertEqual "ifne jump" (Right (mkState [] 10))
    (compIfNe 10 (mkState [VInt 5] 0))),

    TestCase(assertEqual "ifne no jump" (Right (mkState [] 1))
    (compIfNe 10 (mkState [VInt 0] 0))),

    TestCase(assertEqual "ifne wrong type"
        (Left "IIfNe: expected int on stack")
        (compIfNe 10 (mkState [] 0)))
    ]

testIfLt :: Test
testIfLt = TestList [
    TestCase(assertEqual "iflt jump"
        (Right (mkState [] 10))
        (compIfLt 10 (mkState [VInt (-1)] 0))),

    TestCase(assertEqual "iflt no jump"
        (Right (mkState [] 1))
        (compIfLt 10 (mkState [VInt 5] 0))),

    TestCase(assertEqual "iflt wrong type"
        (Left "IIfLt: expected int on stack")
        (compIfLt 10 (mkState [] 0)))
    ]


testIfGe :: Test
testIfGe = TestList [
    TestCase(assertEqual "ifge jump"
        (Right (mkState [] 10))
        (compIfGe 10 (mkState [VInt 0] 0))),

    TestCase(assertEqual "ifge no jump"
        (Right (mkState [] 1))
        (compIfGe 10 (mkState [VInt (-1)] 0))),

    TestCase(assertEqual "ifge wrong type"
        (Left "IIfGe: expected int on stack")
        (compIfGe 10 (mkState [] 0)))
    ]


testIfLe :: Test
testIfLe = TestList [
    TestCase(assertEqual "ifle jump"
        (Right (mkState [] 10))
        (compIfLe 10 (mkState [VInt 0] 0))),

    TestCase(assertEqual "ifle no jump"
        (Right (mkState [] 1))
        (compIfLe 10 (mkState [VInt 5] 0))),

    TestCase(assertEqual "ifle wrong type"
        (Left "IIfLe: expected int on stack")
        (compIfLe 10 (mkState [] 0)))
    ]


testIfACmpEq :: Test
testIfACmpEq = TestList [
    TestCase(assertEqual "acmpeq jump"
        (Right (mkState [] 10))
        (compIfACmpEq 10 (mkState [VInt 5, VInt 5] 0))),

    TestCase(assertEqual "acmpeq no jump"
        (Right (mkState [] 1))
        (compIfACmpEq 10 (mkState [VInt 2, VInt 5] 0))),

    TestCase(assertEqual "acmpeq wrong type"
        (Left "IIfACmpEq: expected two refs on stack")
        (compIfACmpEq 10 (mkState [VInt 1] 0)))
    ]

testIfACmpNe :: Test
testIfACmpNe = TestList [
    TestCase(assertEqual "acmpne jump"
        (Right (mkState [] 10))
        (compIfACmpNe 10 (mkState [VInt 2, VInt 5] 0))),

    TestCase(assertEqual "acmpne no jump"
        (Right (mkState [] 1))
        (compIfACmpNe 10 (mkState [VInt 5, VInt 5] 0))),

    TestCase(assertEqual "acmpne wrong type"
        (Left "IIfACmpNe: expected two refs on stack")
        (compIfACmpNe 10 (mkState [VInt 1] 0)))
    ]

testIfICmpGt :: Test
testIfICmpGt = TestList [
    TestCase(assertEqual "icmpgt jump"
        (Right (mkState [] 10))
        (compIfICmpGt 10 (mkState [VInt 1, VInt 5] 0))),

    TestCase(assertEqual "icmpgt no jump"
        (Right (mkState [] 1))
        (compIfICmpGt 10 (mkState [VInt 5, VInt 5] 0))),

    TestCase(assertEqual "icmpgt wrong type"
        (Left "IIfICmpGt: expected two ints on stack")
        (compIfICmpGt 10 (mkState [VInt 1] 0)))
    ]

testIfICmpEq :: Test
testIfICmpEq = TestList [
    TestCase(assertEqual "icmpeq jump"
        (Right (mkState [] 10))
        (compIfICmpEq 10 (mkState [VInt 5, VInt 5] 0))),

    TestCase(assertEqual "icmpeq no jump"
        (Right (mkState [] 1))
        (compIfICmpEq 10 (mkState [VInt 2, VInt 5] 0))),

    TestCase(assertEqual "icmpeq wrong type"
        (Left "IIfICmpEq: expected two ints on stack")
        (compIfICmpEq 10 (mkState [VInt 1] 0)))
    ]

testIfICmpNe :: Test
testIfICmpNe = TestList [
    TestCase(assertEqual "icmpne jump"
        (Right (mkState [] 10))
        (compIfICmpNe 10 (mkState [VInt 2, VInt 5] 0))),

    TestCase(assertEqual "icmpne no jump"
        (Right (mkState [] 1))
        (compIfICmpNe 10 (mkState [VInt 5, VInt 5] 0))),

    TestCase(assertEqual "icmpne wrong type"
        (Left "IIfICmpNe: expected two ints on stack")
        (compIfICmpNe 10 (mkState [VInt 1] 0)))
    ]

testIfICmpGe :: Test
testIfICmpGe = TestList [
    TestCase(assertEqual "icmpge jump"
        (Right (mkState [] 10))
        (compIfICmpGe 10 (mkState [VInt 5, VInt 5] 0))),
    
    TestCase(assertEqual "icmpge wrong type"
        (Left "IIfICmpGe: expected two ints on stack")
        (compIfICmpGe 10 (mkState [VInt 1] 0)))
    ]

testIfICmpLe :: Test
testIfICmpLe = TestList [
    TestCase(assertEqual "icmple jump"
        (Right (mkState [] 10))
        (compIfICmpLe 10 (mkState [VInt 5, VInt 5] 0))),

    TestCase(assertEqual "icmple wrong type"
        (Left "IIfICmpLe: expected two ints on stack")
        (compIfICmpLe 10 (mkState [VInt 1] 0)))
    ]

testLCmp :: Test
testLCmp = TestList [
    TestCase(assertEqual "lcmp a<b"
        (Right (mkState [VInt (-1)] 1))
        (compLCmp (mkState [VLong 10, VLong 5] 0))),

    TestCase(assertEqual "lcmp a>b"
        (Right (mkState [VInt 1] 1))
        (compLCmp (mkState [VLong 5, VLong 10] 0))),

    TestCase(assertEqual "lcmp a=b"
        (Right (mkState [VInt 0] 1))
        (compLCmp (mkState [VLong 5, VLong 5] 0))),

    TestCase(assertEqual "lcmp wrong type"
        (Left "lcmp expects two longs")
        (compLCmp (mkState [VLong 5] 0)))
    ]

testFCmpL :: Test
testFCmpL = TestList [
    TestCase(assertEqual "fcmpl a<b"
        (Right (mkState [VInt (-1)] 1))
        (compFCmpL (mkState [VFloat 10, VFloat 5] 0))),

    TestCase(assertEqual "fcmpl a>b"
        (Right (mkState [VInt 1] 1))
        (compFCmpL (mkState [VFloat 5, VFloat 10] 0))),

    TestCase(assertEqual "fcmpl a=b"
        (Right (mkState [VInt 0] 1))
        (compFCmpL (mkState [VFloat 5, VFloat 5] 0))),

    TestCase(assertEqual "fcmpl NaN"
        (Right (mkState [VInt (-1)] 1))
        (compFCmpL (mkState [VFloat (0/0), VFloat 5] 0))),

    TestCase(assertEqual "fcmpl wrong type"
        (Left "fcmpl expects two floats")
        (compFCmpL (mkState [VFloat 5] 0)))
    ]

testDCmpG :: Test
testDCmpG = TestList [
    TestCase(assertEqual "dcmpg a<b"
        (Right (mkState [VInt (-1)] 1))
        (compDCmpG (mkState [VDouble 10, VDouble 5] 0))),

    TestCase(assertEqual "dcmpg a>b"
        (Right (mkState [VInt 1] 1))
        (compDCmpG (mkState [VDouble 5, VDouble 10] 0))),

    TestCase(assertEqual "dcmpg a=b"
        (Right (mkState [VInt 0] 1))
        (compDCmpG (mkState [VDouble 5, VDouble 5] 0))),

    TestCase(assertEqual "dcmpg NaN"
        (Right (mkState [VInt 1] 1))
        (compDCmpG (mkState [VDouble (0/0), VDouble 5] 0))),

    TestCase(assertEqual "dcmpg wrong type"
        (Left "dcmpg expects two doubles")
        (compDCmpG (mkState [VDouble 5] 0)))
    ]

testVMConds :: Test
testVMConds = TestList [
    testIfNe,
    testIfLt,
    testIfGe,
    testIfLe,
    testIfACmpEq,
    testIfACmpNe,
    testIfICmpGt,
    testIfICmpEq,
    testIfICmpNe,
    testIfICmpGe,
    testIfICmpLe,
    testLCmp,
    testFCmpL,
    testDCmpG
    ]
