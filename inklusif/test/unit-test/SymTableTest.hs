{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- CompilerTest.hs
-}

module SymTableTest (
    testSymTable
) where
import SymbolTableUtils (getVarType, getVarIndex)
import Test.HUnit
import Ast (Type(..))
import CompilerTypes (SymInfo(..))

testSymTableVars :: Test
testSymTableVars = TestList[
    TestCase (assertEqual "getVarIndex existing variable"
        (Right 1)
        (getVarIndex "foo" ([], (0, [], [], [], [], 0), [], [("foo", (SymInfo 1 IntType Nothing False))]))
    ),
    TestCase (assertEqual "getVarIndex non-existing variable"
        (Left "Variable bar does not exist.")
        (getVarIndex "bar" ([], (0, [], [], [], [], 0), [], [("foo", (SymInfo 1 IntType Nothing False))]))
    ),
    TestCase (assertEqual "getVarType existing variable"
        (Right IntType)
        (getVarType "foo" ([], (0, [], [], [], [], 0), [], [("foo", (SymInfo 1 IntType Nothing False))]))
    ),
    TestCase (assertEqual "getVarType non-existing variable"
        (Left "Variable bar does not exist.")
        (getVarType "bar" ([], (0, [], [], [], [], 0), [], [("foo", (SymInfo 1 IntType Nothing False))])))
    ]

testSymTable :: Test
testSymTable = TestList[
    testSymTableVars
    ]
