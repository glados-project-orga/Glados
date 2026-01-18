module VmTest2 (
    testControlFlow
) where

import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data
import ControlFlowInstr

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

mkFunc :: [Instr] -> Function
mkFunc code = Function
    { funcName = "test"
    , funcCode = V.fromList code
    }

nop :: Instr
nop = IStck INop

testReturn :: Test
testReturn = TestList [
    TestCase(assertEqual "return no frame"
        (Right (mkState [] 10){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })
        (controlFlowReturn (mkState [] 0){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })),

    TestCase(assertEqual "return with frame"
        (Right (mkState [] 5){ currentFunc = "caller", frames = [], locals = V.fromList [VInt 1] })
        (controlFlowReturn (mkState [] 0){ currentFunc = "callee", frames = [Frame [VInt 1] 5 "caller"] }))
    ]

testReturnA :: Test
testReturnA = TestList [
    TestCase(assertEqual "areturn no frame"
        (Right (mkState [VHandle 3] 10){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })
        (controlFlowReturnA (mkState [VHandle 3] 0){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })),

    TestCase(assertEqual "areturn wrong type"
        (Left "AReturn: expected a reference (VHandle) on the stack")
        (controlFlowReturnA (mkState [VInt 5] 0)))
    ]

testReturnInt :: Test
testReturnInt = TestList [
    TestCase(assertEqual "ireturn no frame"
        (Right (mkState [VInt 7] 10){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })
        (controlFlowReturnInt (mkState [VInt 7] 0){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })),

    TestCase(assertEqual "ireturn with frame"
        (Right (mkState [VInt 7, VInt 9] 5){ currentFunc = "caller", frames = [], locals = V.fromList [VInt 1] })
        (controlFlowReturnInt (mkState [VInt 7, VInt 9] 0){ currentFunc = "callee", frames = [Frame [VInt 1] 5 "caller"] }))
    ]

testReturnFloat :: Test
testReturnFloat = TestList [
    TestCase(assertEqual "freturn no frame"
        (Right (mkState [VFloat 1.5] 10){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })
        (controlFlowReturnFloat (mkState [VFloat 1.5] 0){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })),

    TestCase(assertEqual "freturn wrong type"
        (Left "IReturnFloat expects an float on top of the stack")
        (controlFlowReturnFloat (mkState [VInt 5] 0)))
    ]

testReturnDouble :: Test
testReturnDouble = TestList [
    TestCase(assertEqual "dreturn no frame"
        (Right (mkState [VDouble 2.5] 10){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })
        (controlFlowReturnDouble (mkState [VDouble 2.5] 0){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })),

    TestCase(assertEqual "dreturn wrong type"
        (Left "IReturnIDouble expects an double on top of the stack")
        (controlFlowReturnDouble (mkState [VInt 5] 0)))
    ]

testReturnLong :: Test
testReturnLong = TestList [
    TestCase(assertEqual "lreturn no frame"
        (Right (mkState [VLong 99] 10){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })
        (controlFlowReturnLong (mkState [VLong 99] 0){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })),

    TestCase(assertEqual "lreturn wrong type"
        (Left "IReturnLong expects an long on top of the stack")
        (controlFlowReturnLong (mkState [VInt 5] 0)))
    ]

testReturnChar :: Test
testReturnChar = TestList [
    TestCase(assertEqual "creturn no frame"
        (Right (mkState [VChar 'x'] 10){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })
        (controlFlowReturnChar (mkState [VChar 'x'] 0){ functions = Map.fromList [("main", mkFunc (replicate 10 nop))], currentFunc = "main" })),

    TestCase(assertEqual "creturn wrong type"
        (Left "IReturnChar expects a char on top of the stack")
        (controlFlowReturnChar (mkState [VInt 5] 0)))
    ]

testGoto :: Test
testGoto = TestCase(assertEqual "goto"
    (Right (mkState [] 42))
    (controlFlowGoto 42 (mkState [] 0)))

testInvokeStatic :: Test
testInvokeStatic = TestList [
    TestCase(assertEqual "invoke static ok"
        (Right (mkState [] 0)
            { currentFunc = "foo", frames = [Frame [] 1 "main"]
                , functions = Map.fromList [("foo", mkFunc [])]
            })
        (controlFlowInvokeStatic "foo"
            (mkState [] 0){ currentFunc = "main", functions = Map.fromList [("foo", mkFunc [])] }))
    ]

testControlFlow :: Test
testControlFlow = TestList [
    testReturn,
    testReturnA,
    testReturnInt,
    testReturnFloat,
    testReturnDouble,
    testReturnLong,
    testReturnChar,
    testGoto,
    testInvokeStatic
    ]
