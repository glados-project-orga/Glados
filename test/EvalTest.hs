{-
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- EvalTest.hs
-}

module EvalTest (
    testEval
) where

import Test.HUnit
import Eval(
    evalAST,
    applyBuiltin,
    applyLambda,
    evalASTAcall1,
    evalASTAcall2,
    Env
    )

import Data(Ast(..))
import qualified Data.Map as Map

emptyEnv :: Env
emptyEnv = Map.empty

intEnv :: Env
intEnv = Map.fromList[("x", AInt 1)]

testEvalAInt :: Test
testEvalAInt = TestList [
    TestCase (assertEqual "Eval simple AInt" (Right (emptyEnv, AInt 1)) (evalAST emptyEnv (AInt 1)))
    ]

testEvalABool :: Test
testEvalABool = TestList [
    TestCase (assertEqual "Eval simple ABool" (Right (emptyEnv, ABool True)) (evalAST emptyEnv (ABool True)))
    ]

testEvalSymbol :: Test
testEvalSymbol = TestList [
    TestCase (assertEqual "Eval bound symbol" (Right (intEnv, AInt 1)) (evalAST intEnv (ASymbol "x"))),
    TestCase (assertEqual "Eval unbound symbol" (Left "Unbound symbol: x") (evalAST emptyEnv (ASymbol "x")))
    ]

testEvalDefine :: Test
testEvalDefine = TestList [
    TestCase (assertEqual "Eval define simple int" (Right (intEnv, AVoid))(evalAST emptyEnv (ADefine "x" (AInt 1)))),
    TestCase (assertEqual "Eval define error" (Left "Unbound symbol: ") (evalAST emptyEnv (ADefine "x" (ASymbol ""))))
    ]

testEval :: Test
testEval = TestList[
    testEvalAInt,
    testEvalABool,
    testEvalSymbol,
    testEvalDefine
    ]
