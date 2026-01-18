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
    getClass,
    getClasses,
    getClassVarType,
    isClassDefined,
    getNuancedArray,
    isArrayGivenType,
    isArrayMixed,
    getLitArrayType,
    getArraySubType
    )
import Ast (Type(..),
    Literal(..),
    Expr(..),
    ArrayVar(..),
    TypedefDecl(..),
    SourcePos(..),
    Declaration(..),
    ClassDecl(..),
    StructField(..),
    )
import CompilerTypes (Search(..),
    CompilerData,
    SymInfo(..),
    Defines
    )

emptyCompilerData :: CompilerData
emptyCompilerData = ([], (0, [], [], [], [], 0), [], [])

testClass1 :: ClassDecl
testClass1 = ClassDecl 
    (SourcePos 1 1)
    "TestClass"
    [StructField "field1" IntType, StructField "field2" StringType]
    []

testClass2 :: ClassDecl
testClass2 = ClassDecl
    (SourcePos 2 1)
    "AnotherClass"
    [StructField "x" FloatType, StructField "y" FloatType]
    []

compilerDataWithClasses :: CompilerData
compilerDataWithClasses = ([], (0, [], [testClass1, testClass2], [], [], 0), [], [])

emptyDefines :: Defines
emptyDefines = (0, [], [], [], [], 0)

definesWithClasses :: Defines
definesWithClasses = (0, [], [testClass1, testClass2], [], [], 0)

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

testClassTools :: Test
testClassTools = TestList[
    TestCase (assertEqual "getClasses - empty"
        []
        (getClasses emptyCompilerData)
    ),
    TestCase (assertEqual "getClasses - with classes"
        [testClass1, testClass2]
        (getClasses compilerDataWithClasses)
    ),
    TestCase (assertEqual "getClass - existing class"
        (Right testClass1)
        (getClass "TestClass" compilerDataWithClasses)
    ),
    TestCase (assertEqual "getClass - non-existing class"
        (Left "Class NonExistent does not exist.")
        (getClass "NonExistent" compilerDataWithClasses)
    ),
    TestCase (assertEqual "getClass - from empty"
        (Left "Class TestClass does not exist.")
        (getClass "TestClass" emptyCompilerData)
    ),
    TestCase (assertEqual "getClassVarType - existing field"
        (Right IntType)
        (getClassVarType "TestClass" "field1" compilerDataWithClasses)
    ),
    TestCase (assertEqual "getClassVarType - another existing field"
        (Right StringType)
        (getClassVarType "TestClass" "field2" compilerDataWithClasses)
    ),
    TestCase (assertEqual "getClassVarType - field from different class"
        (Right FloatType)
        (getClassVarType "AnotherClass" "x" compilerDataWithClasses)
    ),
    TestCase (assertEqual "getClassVarType - non-existing field"
        (Left "Variable nonExistentField does not exist in class TestClass.")
        (getClassVarType "TestClass" "nonExistentField" compilerDataWithClasses)
    ),
    TestCase (assertEqual "getClassVarType - non-existing class"
        (Left "Class NonExistent does not exist.")
        (getClassVarType "NonExistent" "field1" compilerDataWithClasses)
    ),
    TestCase (assertEqual "isClassDefined - existing class"
        True
        (isClassDefined "TestClass" definesWithClasses)
    ),
    TestCase (assertEqual "isClassDefined - another existing class"
        True
        (isClassDefined "AnotherClass" definesWithClasses)
    ),
    TestCase (assertEqual "isClassDefined - non-existing class"
        False
        (isClassDefined "NonExistent" definesWithClasses)
    ),
    TestCase (assertEqual "isClassDefined - empty defines"
        False
        (isClassDefined "TestClass" emptyDefines)
    )
    ]

testArrayUtils :: Test
testArrayUtils = TestList[
    TestCase (assertEqual "getNuancedArray - empty array"
        ([], [])
        (getNuancedArray [] emptyCompilerData)
    ),
    TestCase (assertEqual "getNuancedArray - all valid int expressions"
        ([], [IntType, IntType, IntType])
        (getNuancedArray [LitExpr (IntLit 1), LitExpr (IntLit 2), LitExpr (IntLit 3)] emptyCompilerData)
    ),
    TestCase (assertEqual "getNuancedArray - mixed valid types"
        ([], [IntType, FloatType, BoolType])
        (getNuancedArray [LitExpr (IntLit 1), LitExpr (FloatLit 2.5), LitExpr (BoolLit True)] emptyCompilerData)
    ),

    TestCase (assertEqual "isArrayGivenType - VoidType with empty list"
        True
        (isArrayGivenType VoidType [])
    ),
    TestCase (assertEqual "isArrayGivenType - all IntType"
        True
        (isArrayGivenType IntType [IntType, IntType, IntType])
    ),
    TestCase (assertEqual "isArrayGivenType - mixed types"
        False
        (isArrayGivenType IntType [IntType, FloatType, IntType])
    ),
    TestCase (assertEqual "isArrayGivenType - all FloatType"
        True
        (isArrayGivenType FloatType [FloatType, FloatType])
    ),
    TestCase (assertEqual "isArrayGivenType - wrong type"
        False
        (isArrayGivenType IntType [FloatType, FloatType])
    ),
    TestCase (assertEqual "isArrayMixed - empty array"
        False
        (isArrayMixed [])
    ),
    TestCase (assertEqual "isArrayMixed - single type"
        False
        (isArrayMixed [IntType])
    ),
    TestCase (assertEqual "isArrayMixed - all same type"
        False
        (isArrayMixed [IntType, IntType, IntType])
    ),
    TestCase (assertEqual "isArrayMixed - mixed types"
        True
        (isArrayMixed [IntType, FloatType, IntType])
    ),
    TestCase (assertEqual "isArrayMixed - two different types"
        True
        (isArrayMixed [IntType, BoolType])
    ),
    TestCase (assertEqual "getLitArrayType - empty array"
        (Right (ArrayType (ArrayVar VoidType (LitExpr (IntLit 0)))))
        (getLitArrayType [] emptyCompilerData)
    ),
    TestCase (assertEqual "getLitArrayType - all int literals"
        (Right (ArrayType (ArrayVar IntType (LitExpr (IntLit 0)))))
        (getLitArrayType [LitExpr (IntLit 1), LitExpr (IntLit 2), LitExpr (IntLit 3)] emptyCompilerData)
    ),
    TestCase (assertEqual "getLitArrayType - all float literals"
        (Right (ArrayType (ArrayVar FloatType (LitExpr (IntLit 0)))))
        (getLitArrayType [LitExpr (FloatLit 1.0), LitExpr (FloatLit 2.5)] emptyCompilerData)
    ),
    TestCase (assertEqual "getLitArrayType - all bool literals"
        (Right (ArrayType (ArrayVar BoolType (LitExpr (IntLit 0)))))
        (getLitArrayType [LitExpr (BoolLit True), LitExpr (BoolLit False)] emptyCompilerData)
    ),
    TestCase (assertEqual "getLitArrayType - all char literals"
        (Right (ArrayType (ArrayVar CharType (LitExpr (IntLit 0)))))
        (getLitArrayType [LitExpr (CharLit 'a'), LitExpr (CharLit 'b')] emptyCompilerData)
    ),
    TestCase (assertEqual "getLitArrayType - mixed types error"
        (Left "Array contains mixed expression types.")
        (getLitArrayType [LitExpr (IntLit 1), LitExpr (FloatLit 2.5)] emptyCompilerData)
    ),
    TestCase (assertEqual "getArraySubType - IntType array"
        (Right IntType)
        (getArraySubType (ArrayType (ArrayVar IntType (LitExpr (IntLit 10)))))
    ),
    TestCase (assertEqual "getArraySubType - FloatType array"
        (Right FloatType)
        (getArraySubType (ArrayType (ArrayVar FloatType (LitExpr (IntLit 5)))))
    ),
    TestCase (assertEqual "getArraySubType - CharType array"
        (Right CharType)
        (getArraySubType (ArrayType (ArrayVar CharType (LitExpr (IntLit 20)))))
    ),
    TestCase (assertEqual "getArraySubType - non-array type error"
        (Left "Type is not an array type when searching array type.")
        (getArraySubType IntType)
    ),
    TestCase (assertEqual "getArraySubType - nested array"
        (Right (ArrayType (ArrayVar IntType (LitExpr (IntLit 5)))))
        (getArraySubType (ArrayType (ArrayVar (ArrayType (ArrayVar IntType (LitExpr (IntLit 5)))) (LitExpr (IntLit 10)))))
    )
    ]

testCompilerTools :: Test
testCompilerTools = TestList[
    testValidAssignments,
    testConvertToType,
    testAppends,
    testClassTools,
    testArrayUtils
    ]
