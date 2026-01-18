{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- IfTest.hs
-}

module IfTest (testIf) where

import Test.HUnit
import If (compileIf)
import CompilerTypes (CompilerData, Defines)
import Ast
import Data.List (isPrefixOf, isSuffixOf)

emptyDefs :: Defines
emptyDefs = (0, [], [], [], [], 0)

emptyProg :: CompilerData
emptyProg = ([], emptyDefs, [], [])

getBC :: CompilerData -> [String]
getBC (_, _, bc, _) = bc

hasSuffix :: String -> [String] -> Bool
hasSuffix s xs =
  case xs of
    []     -> False
    (y:ys) -> (s `isSuffixOf` y) || hasSuffix s ys

countPrefix :: String -> [String] -> Int
countPrefix p xs =
  case xs of
    []     -> 0
    (y:ys) ->
      case p `isPrefixOf` y of
        True  -> 1 + countPrefix p ys
        False -> countPrefix p ys

compileStmtFail :: Statement -> CompilerData -> Either String CompilerData
compileStmtFail _ _ = Left "stmt fail"

compileStmtOk :: Statement -> CompilerData -> Either String CompilerData
compileStmtOk _ prog = Right prog

testIf :: Test
testIf =
  TestLabel "If.compileIf" $
    TestList
      [ TestLabel "no else emits one ifeq and one label" testIfNoElseStructure
      , TestLabel "with else emits ifeq + goto and two labels" testIfWithElseStructure
      , TestLabel "propagates error from condition compilation" testIfConditionError
      , TestLabel "propagates error from then block (no else)" testIfThenBlockErrorNoElse
      , TestLabel "propagates error from else block (with else)" testIfElseBlockError
      ]

testIfNoElseStructure :: Test
testIfNoElseStructure =
  TestCase $
    case compileIf compileStmtOk ifs emptyProg of
      Left err -> assertFailure ("Expected Right, got Left: " ++ err)
      Right prog ->
        let bc = getBC prog
        in case (countPrefix "ifeq " bc == 1, hasSuffix ":" bc) of
             (True, True) -> return ()
             (False, _)   -> assertFailure ("Expected exactly one 'ifeq', got: " ++ show bc)
             (_, False)   -> assertFailure ("Expected an end label (suffix ':'), got: " ++ show bc)
  where
    ifs = IfStmt
      { ifCondition = LitExpr (BoolLit True)
      , ifThenBody  = []
      , ifElseBody  = Nothing
      }

testIfWithElseStructure :: Test
testIfWithElseStructure =
  TestCase $
    case compileIf compileStmtOk ifs emptyProg of
      Left err -> assertFailure ("Expected Right, got Left: " ++ err)
      Right prog ->
        let bc = getBC prog
            nLabels = countSuffix ":" bc
        in case (countPrefix "ifeq " bc == 1, countPrefix "goto " bc == 1, nLabels >= 2) of
             (True, True, True) -> return ()
             (False, _, _)      -> assertFailure ("Expected exactly one 'ifeq', got: " ++ show bc)
             (_, False, _)      -> assertFailure ("Expected exactly one 'goto', got: " ++ show bc)
             (_, _, False)      -> assertFailure ("Expected at least 2 labels (suffix ':'), got: " ++ show bc)
  where
    ifs = IfStmt
      { ifCondition = LitExpr (BoolLit True)
      , ifThenBody  = []
      , ifElseBody  = Just []
      }

countSuffix :: String -> [String] -> Int
countSuffix s xs =
  case xs of
    []     -> 0
    (y:ys) ->
      case s `isSuffixOf` y of
        True  -> 1 + countSuffix s ys
        False -> countSuffix s ys

testIfConditionError :: Test
testIfConditionError =
  TestCase $
    case compileIf compileStmtOk ifs emptyProg of
      Left _  -> return ()
      Right _ -> assertFailure "Expected Left error from condition compilation, got Right"
  where
    ifs = IfStmt
      { ifCondition = CallExpression (CallExpr "nope" [])
      , ifThenBody  = []
      , ifElseBody  = Nothing
      }

testIfThenBlockErrorNoElse :: Test
testIfThenBlockErrorNoElse =
  TestCase $
    case compileIf compileStmtFail ifs emptyProg of
      Left _  -> return ()
      Right _ -> assertFailure "Expected Left error from then block compilation, got Right"
  where
    ifs = IfStmt
      { ifCondition = LitExpr (BoolLit True)
      , ifThenBody  = [ExprStatement (ExprStmt (LitExpr (IntLit 1)))]
      , ifElseBody  = Nothing
      }

testIfElseBlockError :: Test
testIfElseBlockError =
  TestCase $
    case compileIf compileStmtFail ifs emptyProg of
      Left _  -> return ()
      Right _ -> assertFailure "Expected Left error from else block compilation, got Right"
  where
    ifs = IfStmt
      { ifCondition = LitExpr (BoolLit True)
      , ifThenBody  = []
      , ifElseBody  = Just [ExprStatement (ExprStmt (LitExpr (IntLit 1)))]
      }
