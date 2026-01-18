{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- CompilerTest.hs
-}

module CompilerTest (
    testCompiler
) where
import CompilerMain (compilerMain, compileAst)
import Test.HUnit
import Ast (Declaration(..), FunctionDecl(..), SourcePos(..), Type(..))
import Data.Either (isRight)

testCompilerMain :: Test
testCompilerMain = TestList[
    TestCase (assertEqual "compilerMain with empty AST" 
        (Right ([], (0, [], [], [], [], 0), [], []))
        (compilerMain [] (Right ([], (0, [], [], [], [], 0), [], [])))
    ),
    TestCase (assertEqual "compilerMain with one function declaration" 
        (Right ([], (0, [testFunc], [], [], [], 0), ["fun foo {\n","}\n"], [])) 
        (compilerMain [Function testFunc] 
            (Right ([], (0, [], [], [], [], 0), [], [])))
        ),
    TestCase (assertBool "compileAst succeeds with function" 
        (isRight (compileAst [Function testFunc]))
    )
    ]
        where testFunc = FunctionDecl (SourcePos 0 0) "foo" [] VoidType []
-- testFoundInt :: Test
-- testFoundInt = TestList[
--     TestCase(assertEqual "foundInt AInt 5" (Right 5) (foundInt (AInt 5))),
--     TestCase(assertEqual "foundInt ASymbol foo" (Left "foo is not a number") (foundInt (ASymbol "foo")))
--     ]


testCompiler :: Test
testCompiler = TestList[
    testCompilerMain
    ]
