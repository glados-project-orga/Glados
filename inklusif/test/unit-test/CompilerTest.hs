{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- CompilerTest.hs
-}

module CompilerTest (
    testCompiler
) where
import CompilerMain (compilerMain)
import Test.HUnit
import Ast (Declaration(..), FunctionDecl(..), SourcePos(..), Type(..))

testCompilerMain :: Test
testCompilerMain = TestList[
    TestCase (assertEqual "compilerMain with empty AST" 
        (Right ([], (_, [], [], [], []), [], []))
        (compilerMain [] (Right ([], (_, [], [], [], []), [], [])))
    ),
    TestCase (assertEqual "compilerMain with one function declaration" 
        (Right ([], (_, [testFunc], [], [], []), ["fun foo {\n","}\n"], [])) 
        (compilerMain [Function testFunc] 
            (Right ([], (_, [], [], [], []), [], []))))
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
