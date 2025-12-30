{-
-- EPITECH PROJECT, 2021
-- epi-repo
-- File description:
-- DataTest.hs
-}

module DataTest (
    testData
) where
import Test.HUnit
import Data(Ast(..), formatList)


testDataShow :: Test
testDataShow = TestList [
    TestCase (assertEqual "Show AInt" "1" (show (AInt 1))),
    TestCase (assertEqual "Show ABool" "#t" (show (ABool True))),
    TestCase (assertEqual "Show ASymbol" "x" (show (ASymbol "x"))),
    TestCase (assertEqual "Show ADefine" "(define x 1)" (show (ADefine "x" (AInt 1)))),
    TestCase (assertEqual "Show ALambda" "(lambda (x ) x)" (show (ALambda ["x"] (ASymbol "x")))),
    TestCase (assertEqual "Show ACall" "(+ 1 2 )" (show (ACall (ASymbol "+") [AInt 1, AInt 2])))
    ]

testformatList :: Test
testformatList = TestList [
    TestCase (assertEqual "Format empty list" "" (formatList [])),
    TestCase (assertEqual "Format single element list" "1" (formatList [AInt 1])),
    TestCase (assertEqual "Format multiple element list with different types" "a 2 3" (formatList [ASymbol "a", AInt 2, AInt 3]))
    ]

testData :: Test
testData = TestList[
    testDataShow,
    testformatList
    ]