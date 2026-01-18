{-
-- EPITECH PROJECT, 2025
-- unit test
-- File description:
-- VMCondTest.hs
-}

module VmTest1 (
    testHeapInstr
) where

import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Vector as V

import Data
import HeapInstr

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

testNewArray :: Test
testNewArray = TestList [
    TestCase(assertEqual "newarray normal"
        (Right (mkState [VInt 0] 1){ heap = V.fromList [HArray (V.replicate 3 (VInt 0))] })
        (heapInstrNewArray (mkState [VInt 3] 0))),

    TestCase(assertEqual "newarray negative"
        (Left "INewArray: negative array size")
        (heapInstrNewArray (mkState [VInt (-1)] 0))),

    TestCase(assertEqual "newarray wrong type"
        (Left "INewArray expects an integer size on the stack")
        (heapInstrNewArray (mkState [] 0)))
    ]

testALoad :: Test
testALoad = TestList [
    TestCase(assertEqual "aload ok"
        (Right (mkState [VInt 5] 1){ heap = V.fromList [HArray (V.fromList [VInt 5])] })
        (heapInstrALoad (mkState [VInt 0, VInt 0] 0){ heap = V.fromList [HArray (V.fromList [VInt 5])] })),

    TestCase(assertEqual "aload OOB"
        (Left "IALoad: array index out of bounds")
        (heapInstrALoad (mkState [VInt 1, VInt 0] 0){ heap = V.fromList [HArray (V.fromList [VInt 5])] })),

    TestCase(assertEqual "aload wrong ref"
        (Left "IALoad: reference is not an array")
        (heapInstrALoad (mkState [VInt 0, VInt 0] 0){ heap = V.fromList [HObject Map.empty] })),

    TestCase(assertEqual "aload wrong stack"
        (Left "IALoad expects (index, arrayRef) on the stack")
        (heapInstrALoad (mkState [VInt 0] 0)))
    ]

testAStore :: Test
testAStore = TestList [
    TestCase(assertEqual "astore ok"
        (Right (mkState [] 1){ heap = V.fromList [HArray (V.fromList [VInt 9])] })
        (heapInstrAStore (mkState [VInt 9, VInt 0, VInt 0] 0){ heap = V.fromList [HArray (V.fromList [VInt 0])] })),

    TestCase(assertEqual "astore OOB"
        (Left "IAStore: array index out of bounds")
        (heapInstrAStore (mkState [VInt 9, VInt 1, VInt 0] 0){ heap = V.fromList [HArray (V.fromList [VInt 0])] })),

    TestCase(assertEqual "astore wrong ref"
        (Left "IAStore: reference is not an array")
        (heapInstrAStore (mkState [VInt 9, VInt 0, VInt 0] 0){ heap = V.fromList [HObject Map.empty] })),

    TestCase(assertEqual "astore wrong stack"
        (Left "IAStore expects (value, index, arrayRef) on the stack")
        (heapInstrAStore (mkState [VInt 9, VInt 0] 0)))
    ]

testArrayLength :: Test
testArrayLength = TestList [
    TestCase(assertEqual "arraylength ok"
        (Right (mkState [VInt 1] 1){ heap = V.fromList [HArray (V.fromList [VInt 0])] })
        (heapInstrArrayLength (mkState [VInt 0] 0){ heap = V.fromList [HArray (V.fromList [VInt 0])] })),

    TestCase(assertEqual "arraylength wrong ref"
        (Left "IArrayLength: reference is not an array")
        (heapInstrArrayLength (mkState [VInt 0] 0){ heap = V.fromList [HObject Map.empty] })),

    TestCase(assertEqual "arraylength wrong stack"
        (Left "IArrayLength expects an array reference ( VInt) on the stack")
        (heapInstrArrayLength (mkState [] 0)))
    ]

testNew :: Test
testNew = TestList [
    TestCase(assertEqual "new object"
        (Right (mkState [VHandle 0] 1){ heap = V.fromList [HObject Map.empty] })
        (heapInstrNew "MyClass" (mkState [] 0)))
    ]

testGetField :: Test
testGetField = TestList [
    TestCase(assertEqual "getfield ok"
        (Right (mkState [VInt 5] 1){ heap = V.fromList [HObject (Map.fromList [("x", VInt 5)])] })
        (heapInstrGetField "x" (mkState [VHandle 0] 0){ heap = V.fromList [HObject (Map.fromList [("x", VInt 5)])] })),

    TestCase(assertEqual "getfield missing"
        (Left "Field not found: x")
        (heapInstrGetField "x" (mkState [VHandle 0] 0){ heap = V.fromList [HObject Map.empty] })),

    TestCase(assertEqual "getfield wrong ref"
        (Left "getfield: expected object, got array")
        (heapInstrGetField "x" (mkState [VHandle 0] 0){ heap = V.fromList [HArray (V.fromList [])] })),

    TestCase(assertEqual "getfield invalid handle"
        (Left "Invalid heap handle: 1")
        (heapInstrGetField "x" (mkState [VHandle 1] 0){ heap = V.fromList [HObject Map.empty] })),

    TestCase(assertEqual "getfield wrong stack"
        (Left "IGetField expects an object reference on the stack")
        (heapInstrGetField "x" (mkState [VInt 5] 0)))
    ]

testPutField :: Test
testPutField = TestList [
    TestCase(assertEqual "putfield ok"
        (Right (mkState [] 1){ heap = V.fromList [HObject (Map.fromList [("x", VInt 9)])] })
        (heapInstrPutField "x" (mkState [VInt 9, VHandle 0] 0){ heap = V.fromList [HObject Map.empty] })),

    TestCase(assertEqual "putfield wrong ref"
        (Left "putfield: expected object, got array")
        (heapInstrPutField "x" (mkState [VInt 9, VHandle 0] 0){ heap = V.fromList [HArray (V.fromList [])] })),

    TestCase(assertEqual "putfield invalid handle"
        (Left "Invalid heap handle: 1")
        (heapInstrPutField "x" (mkState [VInt 9, VHandle 1] 0){ heap = V.fromList [HObject Map.empty] })),

    TestCase(assertEqual "putfield missing value"
        (Left "Stack underflow in IPutField (need value and object ref)")
        (heapInstrPutField "x" (mkState [VHandle 0] 0))),

    TestCase(assertEqual "putfield empty stack"
        (Left "Stack underflow in IPutField")
        (heapInstrPutField "x" (mkState [] 0)))
    ]

testHeapInstr :: Test
testHeapInstr = TestList [
    testNewArray,
    testALoad,
    testAStore,
    testArrayLength,
    testNew,
    testGetField,
    testPutField
    ]
