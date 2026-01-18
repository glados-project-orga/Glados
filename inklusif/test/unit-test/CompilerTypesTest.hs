{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- CompilerTypesTest.hs
-}

module CompilerTypesTest (testCompilerTypes) where

import Test.HUnit
import CompilerTypes
import Ast (Type(..), Literal(..), Expr(..), ArrayVar(..))

testCompilerTypes :: Test
testCompilerTypes =
  TestLabel "CompilerTypes" $
    TestList
      [ TestLabel "Convert Literal" testConvertLiteral
      , TestLabel "Convert String" testConvertString
      , TestLabel "TypeEq Type Type (special rules)" testTypeEqTypeType
      , TestLabel "TypeEq Type String" testTypeEqTypeString
      , TestLabel "TypeEq Type Literal" testTypeEqTypeLiteral
      , TestLabel "TypeEq wrappers (Either/Maybe)" testTypeEqWrappers
      , TestLabel "TypeEq TypeNormalized" testTypeEqTypeNormalized
      , TestLabel "Search instances" testSearch
      ]

testConvertLiteral :: Test
testConvertLiteral =
  TestList
    [ "IntLit -> IntType" ~:
        IntType ~=? convert (IntLit 42)
    , "FloatLit -> FloatType" ~:
        FloatType ~=? convert (FloatLit 3.14)
    , "DoubleLit -> DoubleType" ~:
        DoubleType ~=? convert (DoubleLit 2.718)
    , "BoolLit -> BoolType" ~:
        BoolType ~=? convert (BoolLit True)
    , "CharLit -> CharType" ~:
        CharType ~=? convert (CharLit 'a')
    , "LongLit -> LongType" ~:
        LongType ~=? convert (LongLit 10)
    , "StringLit -> ArrayType(Char, len)" ~:
        testConvertStringLitShape
    ]

testConvertStringLitShape :: Test
testConvertStringLitShape =
  TestCase $
    case convert (StringLit "abc") of
      ArrayType (ArrayVar CharType (LitExpr (IntLit 3))) -> return ()
      other -> assertFailure ("Expected string to convert to ArrayType(Char, 3), got: " ++ show other)

testConvertString :: Test
testConvertString =
  TestList
    [ "\"int\" -> IntType" ~: IntType ~=? convert ("int" :: String)
    , "\"float\" -> FloatType" ~: FloatType ~=? convert ("float" :: String)
    , "\"double\" -> DoubleType" ~: DoubleType ~=? convert ("double" :: String)
    , "\"bool\" -> BoolType" ~: BoolType ~=? convert ("bool" :: String)
    , "\"char\" -> CharType" ~: CharType ~=? convert ("char" :: String)
    , "\"void\" -> VoidType" ~: VoidType ~=? convert ("void" :: String)
    , "other -> CustomType" ~:
        CustomType "UserType" ~=? convert ("UserType" :: String)
    ]

testTypeEqTypeType :: Test
testTypeEqTypeType =
  TestList
    [ "LongType == IntType (special)" ~: True ~=? typeEq LongType IntType
    , "IntType == LongType ? (should be False unless also special-cased)" ~:
        False ~=? typeEq IntType LongType

    , "ArrayType(Char, _) == StringType (special)" ~:
        True ~=? typeEq (ArrayType (ArrayVar CharType (LitExpr (IntLit 5)))) StringType

    , "ArrayType(Int, _) == ArrayType(Int, _)" ~:
        True ~=? typeEq
          (ArrayType (ArrayVar IntType (LitExpr (IntLit 1))))
          (ArrayType (ArrayVar IntType (LitExpr (IntLit 99))))

    , "ArrayType(Int, _) == ArrayType(Bool, _) -> False" ~:
        False ~=? typeEq
          (ArrayType (ArrayVar IntType (LitExpr (IntLit 1))))
          (ArrayType (ArrayVar BoolType (LitExpr (IntLit 1))))

    , "CustomType name equality" ~:
        True ~=? typeEq (CustomType "A") (CustomType "A")

    , "CustomType name inequality" ~:
        False ~=? typeEq (CustomType "A") (CustomType "B")
    ]

testTypeEqTypeString :: Test
testTypeEqTypeString =
  TestList
    [ "IntType == \"int\"" ~: True ~=? typeEq IntType ("int" :: String)
    , "VoidType == \"void\"" ~: True ~=? typeEq VoidType ("void" :: String)
    , "Array(Char,_) == \"string\"" ~:
        True ~=? typeEq (ArrayType (ArrayVar CharType (LitExpr (IntLit 1)))) ("string" :: String)

    , "CustomType \"X\" == \"X\"" ~:
        True ~=? typeEq (CustomType "X") ("X" :: String)

    , "BoolType == \"int\" -> False" ~:
        False ~=? typeEq BoolType ("int" :: String)
    ]

testTypeEqTypeLiteral :: Test
testTypeEqTypeLiteral =
  TestList
    [ "IntType matches IntLit" ~: True ~=? typeEq IntType (IntLit 1)
    , "CharType matches CharLit" ~: True ~=? typeEq CharType (CharLit 'x')
    , "BoolType matches BoolLit" ~: True ~=? typeEq BoolType (BoolLit False)
    , "Array(Char,_) matches StringLit" ~:
        True ~=? typeEq (ArrayType (ArrayVar CharType (LitExpr (IntLit 2)))) (StringLit "hi")

    , "IntType matches BoolLit -> False" ~:
        False ~=? typeEq IntType (BoolLit True)
    ]

testTypeEqWrappers :: Test
testTypeEqWrappers =
  TestList
    [ "TypeEq Type (Either String String): Right string" ~:
        True ~=? typeEq IntType (Right "int" :: Either String String)

    , "TypeEq Type (Either String String): Left err" ~:
        False ~=? typeEq IntType (Left "err" :: Either String String)

    , "TypeEq Type (Either String Type): Right type" ~:
        True ~=? typeEq IntType (Right IntType :: Either String Type)

    , "TypeEq Type (Either String Type): Left err" ~:
        False ~=? typeEq IntType (Left "err" :: Either String Type)

    , "TypeEq Type (Maybe Type): Just type" ~:
        True ~=? typeEq IntType (Just IntType :: Maybe Type)

    , "TypeEq Type (Maybe Type): Nothing" ~:
        False ~=? typeEq IntType (Nothing :: Maybe Type)

    , "TypeEq (Either String Type) (Either String Type): Right/Right equal" ~:
        True ~=? typeEq (Right IntType :: Either String Type) (Right IntType :: Either String Type)

    , "TypeEq (Either String Type) (Either String Type): Left anything -> False" ~:
        False ~=? typeEq (Left "e" :: Either String Type) (Right IntType :: Either String Type)
    ]

testTypeEqTypeNormalized :: Test
testTypeEqTypeNormalized =
  TestList
    [ "TypeNorm same -> True" ~:
        True ~=? typeEq (TypeNorm IntType) (TypeNorm IntType)

    , "TypeNorm different -> False" ~:
        False ~=? typeEq (TypeNorm IntType) (TypeNorm BoolType)

    , "LitNorm same -> True" ~:
        True ~=? typeEq (LitNorm (IntLit 1)) (LitNorm (IntLit 1))

    , "TypeNorm vs LitNorm -> False" ~:
        False ~=? typeEq (TypeNorm IntType) (LitNorm (IntLit 1))
    ]

testSearch :: Test
testSearch =
  TestList
    [ "Search Type -> SearchType" ~:
        SearchType IntType ~=? srch IntType

    , "Search Expr -> SearchExpr" ~:
        SearchExpr (LitExpr (IntLit 1)) ~=? srch (LitExpr (IntLit 1))
    ]
