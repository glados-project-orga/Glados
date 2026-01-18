module NormalizeTests (
    testNormalize
) where

import Test.HUnit
import CompilerTools (convertToType)
import CompilerTypes (CompilerData, SymInfo(..))
import Ast (Type(..), Literal(..), Expr(..), ArrayVar(..), BinOp(..), UnaryOp(..), SourcePos(..), EnumDecl(..), EnumField(..))

emptyCompilerData :: CompilerData
emptyCompilerData = ([], (0, [], [], [], [], 0), [], [])

compilerDataWithVar :: CompilerData
compilerDataWithVar = ([], (0, [], [], [], [], 0), [], [("x", SymInfo 0 IntType Nothing False), ("y", SymInfo 1 FloatType Nothing False)])

compilerDataWithArray :: CompilerData
compilerDataWithArray = ([], (0, [], [], [], [], 0), [], [("arr", SymInfo 0 (ArrayType (ArrayVar IntType (LitExpr (IntLit 10)))) Nothing False)])

testEnum :: EnumDecl
testEnum = EnumDecl
    (SourcePos 1 1)
    "Status"
    [EnumField "ACTIVE" (Just 1), EnumField "INACTIVE" (Just 0)]

compilerDataWithEnum :: CompilerData
compilerDataWithEnum = ([], (0, [], [], [testEnum], [], 0), [], [])

testConvertToTypeLiterals :: Test
testConvertToTypeLiterals = TestList[
    TestCase (assertEqual "Convert IntLit to IntType"
        (Right IntType)
        (convertToType (LitExpr (IntLit 42)) emptyCompilerData)
    ),
    TestCase (assertEqual "Convert FloatLit to FloatType"
        (Right FloatType)
        (convertToType (LitExpr (FloatLit 3.14)) emptyCompilerData)
    ),
    TestCase (assertEqual "Convert DoubleLit to DoubleType"
        (Right DoubleType)
        (convertToType (LitExpr (DoubleLit 2.718)) emptyCompilerData)
    ),
    TestCase (assertEqual "Convert BoolLit to BoolType"
        (Right BoolType)
        (convertToType (LitExpr (BoolLit True)) emptyCompilerData)
    ),
    TestCase (assertEqual "Convert CharLit to CharType"
        (Right CharType)
        (convertToType (LitExpr (CharLit 'a')) emptyCompilerData)
    ),
    TestCase (assertEqual "Convert StringLit to ArrayType CharType"
        (Right (ArrayType (ArrayVar CharType (LitExpr (IntLit 5)))))
        (convertToType (LitExpr (StringLit "hello")) emptyCompilerData)
    )
    ]

testConvertToTypeVariables :: Test
testConvertToTypeVariables = TestList[
    TestCase (assertEqual "Convert VarExpr with IntType"
        (Right IntType)
        (convertToType (VarExpr "x") compilerDataWithVar)
    ),
    TestCase (assertEqual "Convert VarExpr with FloatType"
        (Right FloatType)
        (convertToType (VarExpr "y") compilerDataWithVar)
    ),
    TestCase (assertEqual "Convert non-existent VarExpr"
        (Left "Variable z does not exist.")
        (convertToType (VarExpr "z") compilerDataWithVar)
    ),
    TestCase (assertEqual "Convert ArrayVarExpr to element type"
        (Right IntType)
        (convertToType (ArrayVarExpr "arr" (LitExpr (IntLit 0))) compilerDataWithArray)
    )
    ]

testConvertToTypeEnums :: Test
testConvertToTypeEnums = TestList[
    TestCase (assertEqual "Convert enum value to IntType"
        (Right IntType)
        (convertToType (VarExpr "ACTIVE") compilerDataWithEnum)
    ),
    TestCase (assertEqual "Convert another enum value to IntType"
        (Right IntType)
        (convertToType (VarExpr "INACTIVE") compilerDataWithEnum)
    )
    ]

testConvertToTypeBinOp :: Test
testConvertToTypeBinOp = TestList[
    TestCase (assertEqual "BinOp Int + Int = IntType"
        (Right IntType)
        (convertToType (BinOpExpr Add (LitExpr (IntLit 1)) (LitExpr (IntLit 2))) emptyCompilerData)
    ),
    TestCase (assertEqual "BinOp Int + Float = FloatType (priority)"
        (Right FloatType)
        (convertToType (BinOpExpr Add (LitExpr (IntLit 1)) (LitExpr (FloatLit 2.0))) emptyCompilerData)
    ),
    TestCase (assertEqual "BinOp Float + Double = DoubleType (priority)"
        (Right DoubleType)
        (convertToType (BinOpExpr Add (LitExpr (FloatLit 1.0)) (LitExpr (DoubleLit 2.0))) emptyCompilerData)
    ),
    TestCase (assertEqual "BinOp Int + Double = DoubleType (priority)"
        (Right DoubleType)
        (convertToType (BinOpExpr Mul (LitExpr (IntLit 5)) (LitExpr (DoubleLit 2.5))) emptyCompilerData)
    ),
    TestCase (assertEqual "BinOp Bool + Bool = BoolType"
        (Right BoolType)
        (convertToType (BinOpExpr And (LitExpr (BoolLit True)) (LitExpr (BoolLit False))) emptyCompilerData)
    )
    ]

testConvertToTypeUnaryOp :: Test
testConvertToTypeUnaryOp = TestList[
    TestCase (assertEqual "UnaryOp Not returns BoolType"
        (Right BoolType)
        (convertToType (UnaryOpExpr Not (LitExpr (BoolLit True))) emptyCompilerData)
    ),
    TestCase (assertEqual "UnaryOp Neg preserves IntType"
        (Right IntType)
        (convertToType (UnaryOpExpr Neg (LitExpr (IntLit 5))) emptyCompilerData)
    ),
    TestCase (assertEqual "UnaryOp Neg preserves FloatType"
        (Right FloatType)
        (convertToType (UnaryOpExpr Neg (LitExpr (FloatLit 3.14))) emptyCompilerData)
    ),
    TestCase (assertEqual "UnaryOp PreInc preserves IntType"
        (Right IntType)
        (convertToType (UnaryOpExpr PreInc (VarExpr "x")) compilerDataWithVar)
    )
    ]

testConvertToTypeCast :: Test
testConvertToTypeCast = TestList[
    TestCase (assertEqual "Cast Int to Float"
        (Right FloatType)
        (convertToType (CastExpr FloatType (LitExpr (IntLit 42))) emptyCompilerData)
    ),
    TestCase (assertEqual "Cast Float to Int"
        (Right IntType)
        (convertToType (CastExpr IntType (LitExpr (FloatLit 3.14))) emptyCompilerData)
    ),
    TestCase (assertEqual "Cast to CustomType"
        (Right (CustomType "MyType"))
        (convertToType (CastExpr (CustomType "MyType") (LitExpr (IntLit 0))) emptyCompilerData)
    )
    ]

testConvertToTypeArrayLiteral :: Test
testConvertToTypeArrayLiteral = TestList[
    TestCase (assertEqual "ArrayLiteral of ints"
        (Right (ArrayType (ArrayVar IntType (LitExpr (IntLit 0)))))
        (convertToType (ArrayLiteral [LitExpr (IntLit 1), LitExpr (IntLit 2), LitExpr (IntLit 3)]) emptyCompilerData)
    ),
    TestCase (assertEqual "Empty ArrayLiteral"
        (Right (ArrayType (ArrayVar VoidType (LitExpr (IntLit 0)))))
        (convertToType (ArrayLiteral []) emptyCompilerData)
    ),
    TestCase (assertEqual "ArrayLiteral of floats"
        (Right (ArrayType (ArrayVar FloatType (LitExpr (IntLit 0)))))
        (convertToType (ArrayLiteral [LitExpr (FloatLit 1.0), LitExpr (FloatLit 2.0)]) emptyCompilerData)
    ),
    TestCase (assertEqual "Mixed ArrayLiteral error"
        (Left "Array contains mixed expression types.")
        (convertToType (ArrayLiteral [LitExpr (IntLit 1), LitExpr (FloatLit 2.0)]) emptyCompilerData)
    )
    ]

testNormalize :: Test
testNormalize = TestList[
    testConvertToTypeLiterals,
    testConvertToTypeVariables,
    testConvertToTypeEnums,
    testConvertToTypeBinOp,
    testConvertToTypeUnaryOp,
    testConvertToTypeCast,
    testConvertToTypeArrayLiteral
    ]
