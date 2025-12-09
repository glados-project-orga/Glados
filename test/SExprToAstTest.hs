{- 
-- EPITECH PROJECT, 2025
-- SExprToAST.hs
-- File description:
-- Test
-}

module SExprToAstTest (
    testSExpr
) where

import Test.HUnit
import SExprToAST(sexprToAST)
import Data(Ast(..), SExpr(..))

testSExprAint :: Test
testSExprAint = TestList [
    TestCase(assertEqual "SInt to AInt" (Right (AInt 5)) (sexprToAST(SInt 5)))
    ]

testSExprASymbol :: Test
testSExprASymbol = TestList [
    TestCase(assertEqual "SSymbol to ASSymbol" (Right(ASymbol "add")) (sexprToAST(SSymbol "add"))),
    TestCase(assertEqual "SSymbol #t to ABool True" (Right(ABool True)) (sexprToAST(SSymbol "#t"))),
    TestCase(assertEqual "SSymbol #f to ABool False" (Right(ABool False)) (sexprToAST(SSymbol "#f")))
    ]

testSExprADefine :: Test
testSExprADefine = TestList [
    TestCase(assertEqual "SList to ADefine" (Right (ADefine "x" (AInt 5))) (sexprToAST (SList [SSymbol "define", SSymbol "x", SInt 5]))),
    TestCase(assertEqual "SList to ADefine function shorthand"
        (Right (ADefine "add" (ALambda ["a", "b"] (ACall (ASymbol "+") [ASymbol "a", ASymbol "b"]))))
        (sexprToAST (SList [SSymbol "define", SList [SSymbol "add", SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]]))),
    TestCase(assertEqual "Define without body" (Left "Unsupported SExpr form") (sexprToAST (SList [SSymbol "define", SSymbol "x", SList []]))),
    TestCase(assertEqual "Define lambda without args"
        (Left ("Unsupported SExpr form")) (sexprToAST (SList [SSymbol "define", SList [SSymbol "add", SSymbol "a", SSymbol "b"], SList []]))),
    TestCase(assertEqual "Define lambda without body"
        (Left ("Unsupported SExpr form")) (sexprToAST (SList [SSymbol "define", SList [], SList [SSymbol "+", SSymbol "a", SSymbol "b"]])))
    ]

testSExprACalls :: Test
testSExprACalls = TestList [
    TestCase(assertEqual "SList to ACall" (Right (ACall (ASymbol "+") [AInt 5, AInt 6])) (sexprToAST (SList [SSymbol "+", SInt 5, SInt 6]))),
    TestCase(assertEqual "Nested ACall" (Right (ACall (ASymbol "+") [ACall (ASymbol "*") [AInt 2, AInt 3], AInt 4]))
        (sexprToAST (SList [SSymbol "+", SList [SSymbol "*", SInt 2, SInt 3], SInt 4]))),
    TestCase(assertEqual "Call with invalid function" (Left ("Unsupported SExpr form")) (sexprToAST (SList [SList [], SInt 1]))),
    TestCase(assertEqual "Call with empty list" (Left ("Unsupported SExpr form")) (sexprToAST (SList [SSymbol "+", SList []]))),
    TestCase(assertEqual "Call with empty list" (Left ("Unsupported SExpr form")) (sexprToAST (SList [SSymbol "+", SList []])))
    ]

testSExprALambda :: Test
testSExprALambda = TestList [
    TestCase (assertEqual "SList to ALambda"
    (Right (ACall (ALambda ["x"]
        (ASymbol "x")) [AInt 1])) (sexprToAST (SList [ SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "x"], SInt 1 ]))),
    TestCase (assertEqual "Lambda with invalid args"
        (Left "Unsupported SExpr form") (sexprToAST (SList [ SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "x"], SList [] ])))
    ]

testSExprAList :: Test
testSExprAList = TestList [
    TestCase (assertEqual "SList to AList" (Right (AList [AInt 1, AInt 2, AInt 3])) (sexprToAST (SList [SInt 1, SInt 2, SInt 3]))),
    TestCase (assertEqual "Empty SList unsupported" (Left "Unsupported SExpr form") (sexprToAST (SList [])))
    ]

testSExpr :: Test
testSExpr = TestList[
    testSExprACalls,
    testSExprAint,
    testSExprASymbol,
    testSExprADefine,
    testSExprAList,
    testSExprALambda
    ]