
module CallTest (
    testCall
) where

import Test.HUnit
import Expr (compileExpr)
import Ast
import CompilerTypes (CompilerData, Defines)

emptyDefs :: Defines
emptyDefs = (0, [], [], [], [], 0)

emptyProg :: CompilerData
emptyProg = ([], emptyDefs, [], [])

assertLeft :: Either String a -> Assertion
assertLeft res =
  case res of
    Left _  -> return ()
    Right _ -> assertFailure "Expected Left error, got Right"

assertLeftNonEmpty :: Either String a -> Assertion
assertLeftNonEmpty res =
  case res of
    Left err ->
      assertBool "Expected non-empty error message" (not (null err))
    Right _ ->
      assertFailure "Expected Left error, got Right"

testUnknownFunction :: Test
testUnknownFunction =
  TestCase $
    assertLeft $
      compileExpr
        (CallExpression (CallExpr "nope" []))
        emptyProg

testUnknownFunctionWithIntArgs :: Test
testUnknownFunctionWithIntArgs =
  TestCase $
    assertLeft $
      compileExpr
        (CallExpression (CallExpr "nope"
          [ LitExpr (IntLit 1)
          , LitExpr (IntLit 2)
          ]))
        emptyProg

testUnknownFunctionWithMixedArgs :: Test
testUnknownFunctionWithMixedArgs =
  TestCase $
    assertLeft $
      compileExpr
        (CallExpression (CallExpr "nope"
          [ LitExpr (BoolLit True)
          , LitExpr (CharLit 'x')
          , LitExpr (IntLit 42)
          ]))
        emptyProg

testUnknownFunctionWithNestedArg :: Test
testUnknownFunctionWithNestedArg =
  TestCase $
    assertLeft $
      compileExpr
        (CallExpression (CallExpr "nope"
          [ BinOpExpr Add
              (LitExpr (IntLit 1))
              (LitExpr (IntLit 2))
          ]))
        emptyProg

testUnknownFunctionManyArgs :: Test
testUnknownFunctionManyArgs =
  TestCase $
    assertLeft $
      compileExpr
        (CallExpression (CallExpr "nope"
          [ LitExpr (IntLit 0)
          , LitExpr (IntLit 1)
          , LitExpr (IntLit 2)
          , LitExpr (IntLit 3)
          , LitExpr (IntLit 4)
          , LitExpr (IntLit 5)
          ]))
        emptyProg

testUnknownFunctionErrorNotEmpty :: Test
testUnknownFunctionErrorNotEmpty =
  TestCase $
    assertLeftNonEmpty $
      compileExpr
        (CallExpression (CallExpr "nope" []))
        emptyProg

testCall :: Test
testCall =
  TestList
    [ TestLabel "Unknown function (no args)" testUnknownFunction
    , TestLabel "Unknown function (int args)" testUnknownFunctionWithIntArgs
    , TestLabel "Unknown function (mixed args)" testUnknownFunctionWithMixedArgs
    , TestLabel "Unknown function (nested arg)" testUnknownFunctionWithNestedArg
    , TestLabel "Unknown function (many args)" testUnknownFunctionManyArgs
    , TestLabel "Unknown function error is non-empty" testUnknownFunctionErrorNotEmpty
    ]
