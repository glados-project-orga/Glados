module CompilerToolsTest (
    testCompilerTools
) where

import Test.HUnit
import CompilerTools (validAssignmentType,
    convertToType,
    appendBody,
    appendDefines,
    appendHeader,
    appendSymbolTable,
    )
import Ast (Type(..), Literal(..), Expr(..), ArrayVar(..), TypedefDecl(..), SourcePos(..), Declaration(..))
import CompilerTypes (Search(..), CompilerData, SymInfo(..))

emptyCompilerData :: CompilerData
emptyCompilerData = ([], (0, [], [], [], [], 0), [], [])

testValidAssignments :: Test
testValidAssignments = TestList[
    TestCase (assertEqual "ValidAssginment IntType to IntType"
        True
        (validAssignmentType (srch IntType) (srch IntType) emptyCompilerData)
    ),
    TestCase (assertEqual "NotValidAssignment IntType to FloatType"
        False
        (validAssignmentType (srch IntType) (srch FloatType) emptyCompilerData)
    ),
    TestCase (assertEqual "ValidAssignment IntLit to IntType"
        True
        (validAssignmentType (srch (LitExpr (IntLit 0))) (srch IntType) emptyCompilerData)
    ),
    TestCase (assertEqual "ValidAssignment ArrayType to ArrayType"
        True
        (validAssignmentType (srch (ArrayType (ArrayVar IntType (LitExpr (IntLit 0)))))
        (srch (ArrayType (ArrayVar IntType (LitExpr (IntLit 0))))) emptyCompilerData)
    ),
    TestCase (assertEqual "InvalidAssignment ArrayType to ArrayType"
        False
        (validAssignmentType (srch (ArrayType (ArrayVar IntType (LitExpr (IntLit 0)))))
        (srch (ArrayType (ArrayVar FloatType (LitExpr (IntLit 0))))) emptyCompilerData)
    )
    ]

testConvertToType :: Test
testConvertToType = TestList[
    TestCase (assertEqual "Convert IntLit to IntType"
        (Right IntType)
        (convertToType (LitExpr (IntLit 5)) emptyCompilerData)
    ),
    TestCase (assertEqual "Convert StringLit to ArrayType CharType"
        (Right (ArrayType (ArrayVar CharType (LitExpr (IntLit 11)))))
        (convertToType (LitExpr (StringLit "Hello World")) emptyCompilerData)
    )
    ]

testAppends :: Test
testAppends = TestList[
    TestCase (assertEqual "Append Body"
        ([], (0, [], [], [], [], 0), ["line1", "line2"], [])
        (appendBody emptyCompilerData ["line1", "line2"])
    ),
    TestCase (assertEqual "Append Defines"
        ([], (0, [], [], [], [TypedefDecl {typedefPos = SourcePos {srcLine = 0, srcColumn = 0}, typedefOriginal = IntType, typedefAlias = "MyType"}], 0), [] , [])
        (appendDefines emptyCompilerData [(Typedef (TypedefDecl (SourcePos 0 0) IntType "MyType" ))])
    ),
    TestCase (assertEqual "Append Header"
        (["header1", "header2"], (0, [], [], [], [], 0), [], [])
        (appendHeader emptyCompilerData ["header1", "header2"])
    ),
    TestCase (assertEqual "Append Symbol Table"
        ([], (0, [], [], [], [], 0), [], [("var1",SymInfo {symIndex = 0, symVal = IntType, symIsConst = Nothing, symIsRef = False})])
        (appendSymbolTable emptyCompilerData [("var1", (SymInfo 0 IntType Nothing False))])
    )
    ]
testCompilerTools :: Test
testCompilerTools = TestList[
    testValidAssignments,
    testConvertToType,
    testAppends
    ]
