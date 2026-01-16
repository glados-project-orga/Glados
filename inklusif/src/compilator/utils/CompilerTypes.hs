{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- comptype
-}

module CompilerTypes (
    ConstantPool,
    Bytecode,
    SymbolTable,
    SymInfo(..),
    Ast,
    CompilerData,
    Defines,
    CompileResult,
    CompileExpr,
    ShowType(..),
    TypeEq(..),
    Convert(..),
    ConvertExpr(..),
    TypeNormalized(..),
    HeapSize,
    Handle,
    SearchTypes(..),
    Search(..)
) where


import Ast (
    Declaration(..),
    FunctionDecl,
    ClassDecl,
    EnumDecl,
    TypedefDecl,
    Expr(..),
    Type(..),
    Literal(..),
    ArrayVar(..)
    )
type Handle = Int
type HeapSize = Int
type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = (HeapSize, [FunctionDecl], [ClassDecl], [EnumDecl], [TypedefDecl], Int)

data TypeNormalized
    = TypeNorm Type
    | LitNorm Literal
    deriving (Show, Eq)

data SearchTypes
    = SearchType Type
    | SearchExpr Expr
    deriving (Show, Eq)

class Search a where
    srch :: a -> SearchTypes

instance Search Type where
    srch t = SearchType t

instance Search Expr where
    srch e = SearchExpr e

class ShowType a where
    showType :: a -> String

class TypeEq a b where
    typeEq :: a -> b -> Bool

class Convert a where
    convert :: a -> Type

class ConvertExpr a where
    convertExpr :: a -> Expr

instance Convert Literal where
    convert (IntLit _)    = IntType
    convert (FloatLit _)  = FloatType
    convert (DoubleLit _) = DoubleType
    convert (BoolLit _)   = BoolType
    convert (CharLit _)   = CharType
    convert (LongLit _)   = LongType
    convert (StringLit s) = ArrayType (ArrayVar CharType (LitExpr (IntLit (length s))))

instance ConvertExpr Type where
    convertExpr IntType = (LitExpr (IntLit 0))
    convertExpr LongType = (LitExpr (LongLit 0))
    convertExpr FloatType = (LitExpr (FloatLit 0.0))
    convertExpr DoubleType = (LitExpr (DoubleLit 0.0))
    convertExpr BoolType = (LitExpr (BoolLit False))
    convertExpr CharType = (LitExpr (CharLit '\0'))
    convertExpr StringType = (LitExpr (StringLit ""))
    convertExpr VoidType = (LitExpr (StringLit "void"))
    convertExpr (LambdaType _) = (LitExpr (StringLit "lambda"))
    convertExpr (ArrayType (ArrayVar t _)) = (ArrayLiteral [convertExpr t])
    convertExpr (CustomType name) = (LitExpr (StringLit name))

instance Convert String where
    convert "int"    = IntType
    convert "float"  = FloatType
    convert "double" = DoubleType
    convert "bool"   = BoolType
    convert "char"   = CharType
    convert "void"   = VoidType
    convert other    = CustomType other

instance TypeEq Type (Either String String) where
    typeEq _ (Left _)      = False
    typeEq val (Right str)     = typeEq val str

instance TypeEq Type (Maybe Type) where
    typeEq _ (Nothing)      = False
    typeEq val (Just typ)     =  val == typ

instance TypeEq Type (Either String Type) where
    typeEq _ (Left _)      = False
    typeEq val (Right cmplVal)     = val == cmplVal

instance TypeEq (Either String Type) (Either String Type) where
    typeEq (Left _) _ = False
    typeEq _ (Left _) = False
    typeEq (Right val1) (Right val2) = val1 == val2

instance TypeEq TypeNormalized TypeNormalized where
    typeEq (TypeNorm t1) (TypeNorm t2) = t1 == t2
    typeEq (LitNorm l1) (LitNorm l2)   = l1 == l2
    typeEq _ _                         = False

instance TypeEq Type String where
    typeEq IntType "int"          = True
    typeEq FloatType "float"      = True
    typeEq DoubleType "double"    = True
    typeEq BoolType "bool"        = True
    typeEq CharType "char"        = True
    typeEq (ArrayType (ArrayVar CharType _)) "string" = True
    typeEq (ArrayType (ArrayVar t _)) arrayVar = typeEq t arrayVar
    typeEq VoidType "void"        = True
    typeEq (CustomType name) str  = name == str
    typeEq _ _                    = False
instance TypeEq Type Literal where
    typeEq (IntType) (IntLit _)          = True
    typeEq (DoubleType) (DoubleLit _)    = True
    typeEq (FloatType) (FloatLit _)      = True
    typeEq (BoolType) (BoolLit _)        = True
    typeEq (CharType) (CharLit _)        = True
    typeEq (ArrayType (ArrayVar CharType _)) (StringLit _) = True
    typeEq _ _                             = False

data SymInfo = SymInfo
  { symIndex :: Int
  , symVal  :: Type
  } deriving (Show, Eq)


type SymbolTable = [(String, SymInfo)]
type CompilerData = (ConstantPool, Defines, Bytecode, SymbolTable)
type CompileResult = Either String CompilerData
type CompileExpr   = Expr -> CompilerData -> CompileResult
