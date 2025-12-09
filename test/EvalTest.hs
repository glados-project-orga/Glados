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
    Env
    )

import Data(Ast(..))
import qualified Data.Map as Map

emptyEnv :: Env
emptyEnv = Map.empty

intEnv :: Env
intEnv = Map.fromList[("x", AInt 1)]

plusFunctionAST :: Ast
plusFunctionAST = ALambda ["a", "b"] (ACall (ASymbol "+") [ASymbol "a", ASymbol "b"])

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

testEvalLambda :: Test
testEvalLambda = TestList [
    TestCase (assertEqual "Eval simple lambda"
        (Right (emptyEnv, plusFunctionAST))
        (evalAST emptyEnv plusFunctionAST))
    ]

testEvalIfThenElse :: Test
testEvalIfThenElse = TestList [
    TestCase (assertEqual "Eval if then True branch"
        (Right (emptyEnv, AInt 1))
        (evalAST emptyEnv (ACall (ASymbol "if") [ABool True, AInt 1, AInt 0]))),
    TestCase (assertEqual "Eval if then False branch"
        (Right (emptyEnv, AInt 0))
        (evalAST emptyEnv (ACall (ASymbol "if") [ABool False, AInt 1, AInt 0]))),
    TestCase (assertEqual "Eval if with bad condition type"
        (Left "if condition must be a boolean, and not: 2")
        (evalAST emptyEnv (ACall (ASymbol "if") [AInt 2, AInt 1, AInt 0])))
    ]

testEvalIfThenNoElse :: Test
testEvalIfThenNoElse = TestList [
    TestCase (assertEqual "Eval if then True branch"
        (Right (emptyEnv, AInt 1))
        (evalAST emptyEnv (ACall (ASymbol "if") [ABool True, AInt 1]))),
    TestCase (assertEqual "Eval if then False branch"
        (Right (emptyEnv, AVoid))
        (evalAST emptyEnv (ACall (ASymbol "if") [ABool False, AInt 1]))),
    TestCase (assertEqual "Eval if with bad condition type"
        (Left "if condition must be a boolean, and not: 2")
        (evalAST emptyEnv (ACall (ASymbol "if") [AInt 2, AInt 1])))
    ]

testEvalCallByName :: Test
testEvalCallByName = TestList [
    TestCase (assertEqual "Eval ACall with builtin function"
        (Right (emptyEnv, AInt 3)) (evalAST emptyEnv (ACall (ASymbol "+") [AInt 1, AInt 2]))),
    TestCase (assertEqual "Eval ACall with unknown function"
        (Left "Unknown function: unknown-func") (evalAST emptyEnv (ACall (ASymbol "unknown-func") [AInt 1])))
    ]

testEval :: Test
testEval = TestList[
    testEvalAInt,
    testEvalABool,
    testEvalSymbol,
    testEvalDefine,
    testEvalLambda,
    testEvalIfThenElse,
    testEvalIfThenNoElse,
    testEvalCallByName
    ]
