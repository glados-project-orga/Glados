{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- MatchTest.hs
-}

module MatchTest (testMatch) where

import Test.HUnit
import Match (compileMatch)
import Ast
import CompilerTypes (CompilerData, Defines)
import Data.List (isPrefixOf, isSuffixOf)

emptyDefs :: Defines
emptyDefs = (0, [], [], [], [], 0)

emptyProg :: CompilerData
emptyProg = ([], emptyDefs, [], [])

getBC :: CompilerData -> [String]
getBC (_, _, bc, _) = bc

countPrefix :: String -> [String] -> Int
countPrefix p xs =
  case xs of
    []     -> 0
    (y:ys) ->
      case p `isPrefixOf` y of
        True  -> 1 + countPrefix p ys
        False -> countPrefix p ys

hasInstr :: String -> [String] -> Bool
hasInstr instr xs =
  case xs of
    []     -> False
    (y:ys) ->
      case y == instr of
        True  -> True
        False -> hasInstr instr ys

hasSuffixLine :: String -> [String] -> Bool
hasSuffixLine s xs =
  case xs of
    []     -> False
    (y:ys) ->
      case s `isSuffixOf` y of
        True  -> True
        False -> hasSuffixLine s ys

testMatch :: Test
testMatch =
  TestLabel "Match.compileMatch" $
    TestList
      [ TestLabel "default only emits pop and end label" testDefaultOnlyPop
      , TestLabel "one literal case emits dup/isub/ifeq/goto/pop" testOneLiteralCaseStructure
      , TestLabel "two literal cases emits two dup and two ifeq" testTwoLiteralCasesStructure
      , TestLabel "two defaults -> Left" testTwoDefaultsError
      , TestLabel "unsupported literal pattern -> Left" testUnsupportedLiteralError
      , TestLabel "scrutinee compile error propagates" testScrutineeError
      ]

testDefaultOnlyPop :: Test
testDefaultOnlyPop =
  TestCase $
    case compileMatch m emptyProg of
      Left err -> assertFailure ("Expected Right, got Left: " ++ err)
      Right prog ->
        let bc = getBC prog
        in case (hasInstr "pop" bc, countPrefix "goto " bc >= 1, hasSuffixLine ":" bc) of
             (True, True, True) -> return ()
             (False, _, _) -> assertFailure ("Expected a 'pop' for default, got: " ++ show bc)
             (_, False, _) -> assertFailure ("Expected at least one 'goto', got: " ++ show bc)
             (_, _, False) -> assertFailure ("Expected an end label (suffix ':'), got: " ++ show bc)
  where
    m = MatchStmt
          (LitExpr (IntLit 1))
          [ MatchCase DefaultPattern (LitExpr (IntLit 42)) ]

testOneLiteralCaseStructure :: Test
testOneLiteralCaseStructure =
  TestCase $
    case compileMatch m emptyProg of
      Left err -> assertFailure ("Expected Right, got Left: " ++ err)
      Right prog ->
        let bc = getBC prog
        in case (hasInstr "dup" bc, hasInstr "isub" bc, countPrefix "ifeq " bc == 1, countPrefix "goto " bc >= 2, countPrefix "pop" bc >= 2) of
             (True, True, True, True, True) -> return ()
             (False, _, _, _, _) -> assertFailure ("Expected 'dup', got: " ++ show bc)
             (_, False, _, _, _) -> assertFailure ("Expected 'isub', got: " ++ show bc)
             (_, _, False, _, _) -> assertFailure ("Expected exactly one 'ifeq', got: " ++ show bc)
             (_, _, _, False, _) -> assertFailure ("Expected at least two 'goto' (next + end), got: " ++ show bc)
             (_, _, _, _, False) -> assertFailure ("Expected at least two 'pop' (case + default), got: " ++ show bc)
  where
    m = MatchStmt
          (LitExpr (IntLit 1))
          [ MatchCase (LiteralPattern (IntLit 1)) (LitExpr (IntLit 10))
          , MatchCase DefaultPattern (LitExpr (IntLit 0))
          ]

testTwoLiteralCasesStructure :: Test
testTwoLiteralCasesStructure =
  TestCase $
    case compileMatch m emptyProg of
      Left err -> assertFailure ("Expected Right, got Left: " ++ err)
      Right prog ->
        let bc = getBC prog
        in case (countPrefix "dup" bc == 2, countPrefix "ifeq " bc == 2, countPrefix "isub" bc == 2, countPrefix "goto " bc >= 3) of
             (True, True, True, True) -> return ()
             (False, _, _, _) -> assertFailure ("Expected exactly two 'dup', got: " ++ show bc)
             (_, False, _, _) -> assertFailure ("Expected exactly two 'ifeq', got: " ++ show bc)
             (_, _, False, _) -> assertFailure ("Expected exactly two 'isub', got: " ++ show bc)
             (_, _, _, False) -> assertFailure ("Expected at least three 'goto' (next/next/end), got: " ++ show bc)
  where
    m = MatchStmt
          (LitExpr (IntLit 2))
          [ MatchCase (LiteralPattern (IntLit 1)) (LitExpr (IntLit 10))
          , MatchCase (LiteralPattern (IntLit 2)) (LitExpr (IntLit 20))
          , MatchCase DefaultPattern (LitExpr (IntLit 0))
          ]

testTwoDefaultsError :: Test
testTwoDefaultsError =
  TestCase $
    case compileMatch m emptyProg of
      Left _  -> return ()
      Right _ -> assertFailure "Expected Left error when there are two DefaultPattern cases"
  where
    m = MatchStmt
          (LitExpr (IntLit 1))
          [ MatchCase DefaultPattern (LitExpr (IntLit 0))
          , MatchCase DefaultPattern (LitExpr (IntLit 1))
          ]

testUnsupportedLiteralError :: Test
testUnsupportedLiteralError =
  TestCase $
    case compileMatch m emptyProg of
      Left _  -> return ()
      Right _ -> assertFailure "Expected Left error for unsupported pattern literal type"
  where
    m = MatchStmt
          (LitExpr (IntLit 1))
          [ MatchCase (LiteralPattern (StringLit "x")) (LitExpr (IntLit 1))
          , MatchCase DefaultPattern (LitExpr (IntLit 0))
          ]

testScrutineeError :: Test
testScrutineeError =
  TestCase $
    case compileMatch m emptyProg of
      Left _  -> return ()
      Right _ -> assertFailure "Expected Left error from scrutinee compileExpr, got Right"
  where
    m = MatchStmt
          (CallExpression (CallExpr "nope" []))
          [ MatchCase DefaultPattern (LitExpr (IntLit 0)) ]
