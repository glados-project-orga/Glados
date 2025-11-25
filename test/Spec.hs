
module Main (main) where
import Test.HUnit
import Ast.LispAst(SExpr(..), getSymbol, getList, getInterger)

testGetSymbol:: Test
testGetSymbol = TestList [
    TestCase(assertEqual "Verificate symbol" (Just ("+")) (getSymbol (SSymbol "+"))),
    TestCase(assertEqual "Will output Nothing" (Nothing) (getSymbol (SInt 8)))
    ]


testGetInterger :: Test
testGetInterger = TestList [
    TestCase(assertEqual "Verificate int" (Just 5) (getInterger (SInt 5))),
    TestCase(assertEqual "Will output Nothing" (Nothing) (getInterger (SSymbol "+")))
    ]
   

testGetList:: Test
testGetList = TestList [
    TestCase(assertEqual "Verificate interger" (Just ([SSymbol "define", SSymbol "x", SInt 5]))
        (getList (SList [SSymbol "+", SInt 1, SInt 2]))),
    TestCase(assertEqual "Will output Nothing" (Nothing) (getList(SSymbol "define")))
    ]


tests :: Test
tests = TestList[testGetSymbol , testGetList, testGetInterger]

main :: IO Counts
main = runTestTT tests
