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
    CompilerVal(..),
    CompileResult,
    CompileExpr,
    ShowType(..),
    TypeEq(..),
    Convert(..),
) where

import Ast (
    Declaration(..),
    FunctionDecl,
    ClassDecl,
    EnumDecl,
    TypedefDecl,
    Expr,
    Type(..),
    Literal(..)
    )

type HeapSize = Int
type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = (HeapSize, [FunctionDecl], [ClassDecl], [EnumDecl], [TypedefDecl])

data CompilerVal
  = IntCmpl Int
  | DoubleCmpl Double
  | FloatCmpl Float
  | BoolCmpl Bool
  | CharCmpl Char
  | ConstCmpl Int
  | LambdaCmpl Int
  | ArrayCmpl Int String
  | CustomCmpl String
  | VoidCmpl
  deriving (Eq, Show)

class ShowType a where
    showType :: a -> String

class TypeEq a b where
    typeEq :: a -> b -> Bool

class Convert a where
    convert :: a -> CompilerVal

instance Convert Type where
    convert IntType        = IntCmpl 0
    convert FloatType      = FloatCmpl 0.0
    convert DoubleType     = DoubleCmpl 0.0
    convert BoolType       = BoolCmpl False
    convert CharType       = CharCmpl '\0'
    convert (ArrayType t)  = ArrayCmpl 0 (show t)
    convert (CustomType n) = CustomCmpl n
    convert VoidType       = VoidCmpl

instance Convert String where
    convert "int"    = IntCmpl 0
    convert "float"  = FloatCmpl 0.0
    convert "double" = DoubleCmpl 0.0
    convert "bool"   = BoolCmpl False
    convert "char"   = CharCmpl '\0'
    convert "void"   = VoidCmpl
    convert other    = CustomCmpl other

instance TypeEq CompilerVal Literal where
    typeEq (IntCmpl _) (IntLit _)          = True
    typeEq (DoubleCmpl _) (DoubleLit _)    = True
    typeEq (FloatCmpl _) (FloatLit _)      = True
    typeEq (BoolCmpl _) (BoolLit _)        = True
    typeEq (CharCmpl _) (CharLit _)        = True
    typeEq (ArrayCmpl _ "char") (StringLit _) = True
    typeEq _ _                             = False

instance TypeEq CompilerVal Type where
    typeEq (IntCmpl _) (IntType)             = True
    typeEq (FloatCmpl _) (FloatType)         = True
    typeEq (BoolCmpl _) (BoolType)           = True
    typeEq (DoubleCmpl _) (DoubleType)       = True
    typeEq (CharCmpl _) (CharType)           = True
    typeEq (LambdaCmpl _) (LambdaType _)     = True
    typeEq (ArrayCmpl _ t1) (ArrayType t2)   = t1 == show t2
    typeEq (CustomCmpl n1) (CustomType n2)     = n1 == n2
    typeEq VoidCmpl VoidType                    = True
    typeEq _ _                                 = False

instance TypeEq CompilerVal CompilerVal where
    typeEq (IntCmpl _) (IntCmpl _)         = True
    typeEq (DoubleCmpl _) (DoubleCmpl _)    = True
    typeEq (FloatCmpl _) (FloatCmpl _)      = True
    typeEq (BoolCmpl _) (BoolCmpl _)        = True
    typeEq (CharCmpl _) (CharCmpl _)        = True
    typeEq (LambdaCmpl _) (LambdaCmpl _)    = True
    typeEq (ArrayCmpl _ t1) (ArrayCmpl _ t2) = t1 == t2
    typeEq (CustomCmpl n1) (CustomCmpl n2)   = n1 == n2
    typeEq VoidCmpl VoidCmpl                  = True
    typeEq _ _                                 = False

instance ShowType CompilerVal where
    showType (IntCmpl _)       = "int"
    showType (DoubleCmpl _)    = "double"
    showType (FloatCmpl _)     = "float"
    showType (BoolCmpl _)      = "bool"
    showType (CharCmpl _)      = "char"
    showType (ConstCmpl _)     = "const"
    showType (LambdaCmpl _)    = "lambda"
    showType (ArrayCmpl _ val) = "array " ++ val
    showType (CustomCmpl name) = name
    showType VoidCmpl          = "void"

data SymInfo = SymInfo
  { symIndex :: Int
  , symVal  :: CompilerVal
  } deriving (Show, Eq)

type SymbolTable = [(String, SymInfo)]
type CompilerData = (ConstantPool, Defines, Bytecode, SymbolTable)
type CompileResult = Either String CompilerData
type CompileExpr   = Expr -> CompilerData -> CompileResult
