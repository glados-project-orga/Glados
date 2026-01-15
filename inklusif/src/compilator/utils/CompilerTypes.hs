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
    ConvertExpr(..),
    TypeNormalized(..),
    HeapSize
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
import Data.Int (Int64)

type HeapSize = Int
type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = (HeapSize, [FunctionDecl], [ClassDecl], [EnumDecl], [TypedefDecl], Int)

data CompilerVal
  = IntCmpl Int
  | LongCmpl Int64
  | DoubleCmpl Double
  | FloatCmpl Float
  | BoolCmpl Bool
  | CharCmpl Char
  | ConstCmpl Int
  | LambdaCmpl Int
  | ArrayCmpl Int CompilerVal
  | CustomCmpl String
  | VoidCmpl
  deriving (Eq, Show)

data TypeNormalized
  = TypeNorm Type
  | LitNorm Literal
  | CmplNorm CompilerVal
  deriving (Show, Eq)

class ShowType a where
    showType :: a -> String

class TypeEq a b where
    typeEq :: a -> b -> Bool

class Convert a where
    convert :: a -> CompilerVal

class ConvertExpr a where
    convertExpr :: a -> Expr

instance Convert Literal where
    convert (IntLit v)    = IntCmpl v
    convert (FloatLit v)  = FloatCmpl v
    convert (DoubleLit v) = DoubleCmpl v
    convert (BoolLit v)   = BoolCmpl v
    convert (CharLit v)   = CharCmpl v
    convert (LongLit v)   = LongCmpl v
    convert (StringLit s) = ArrayCmpl (length s) (CustomCmpl "char")

instance Convert Type where
    convert IntType        = IntCmpl 0
    convert LongType       = LongCmpl 0
    convert FloatType      = FloatCmpl 0.0
    convert DoubleType     = DoubleCmpl 0.0
    convert BoolType       = BoolCmpl False
    convert CharType       = CharCmpl '\0'
    convert (LambdaType _) = LambdaCmpl 0
    convert (StringType)   = (ArrayCmpl 0 (CharCmpl '\0'))
    convert (ArrayType (ArrayVar t _))  = ArrayCmpl 0 (convert t)
    convert (CustomType n) = CustomCmpl n
    convert VoidType       = VoidCmpl

instance ConvertExpr CompilerVal where
    convertExpr (IntCmpl _) = (LitExpr (IntLit 0))
    convertExpr (LongCmpl _) = (LitExpr (LongLit 0))
    convertExpr (FloatCmpl _) = (LitExpr (FloatLit 0.0))
    convertExpr (DoubleCmpl _) = (LitExpr (DoubleLit 0.0))
    convertExpr (BoolCmpl _) = (LitExpr (BoolLit False))
    convertExpr (CharCmpl _) = (LitExpr (CharLit '\0'))
    convertExpr VoidCmpl = (LitExpr (StringLit "void"))
    convertExpr (ConstCmpl _) = (LitExpr (IntLit 0))
    convertExpr (LambdaCmpl _) = (LitExpr (StringLit "lambda"))
    convertExpr (ArrayCmpl _ valCmpl) = (ArrayLiteral [convertExpr valCmpl])
    convertExpr (CustomCmpl name) = (LitExpr (StringLit name))

instance Convert String where
    convert "int"    = IntCmpl 0
    convert "float"  = FloatCmpl 0.0
    convert "double" = DoubleCmpl 0.0
    convert "bool"   = BoolCmpl False
    convert "char"   = CharCmpl '\0'
    convert "void"   = VoidCmpl
    convert other    = CustomCmpl other

instance TypeEq CompilerVal (Either String String) where
    typeEq _ (Left _)      = False
    typeEq val (Right str)     = typeEq val str

instance TypeEq CompilerVal (Maybe Type) where
    typeEq _ (Nothing)      = False
    typeEq val (Just typ)     = typeEq val typ

instance TypeEq CompilerVal (Either String CompilerVal) where
    typeEq _ (Left _)      = False
    typeEq val (Right cmplVal)     = typeEq val cmplVal

instance TypeEq (Either String CompilerVal) (Either String CompilerVal) where
    typeEq (Left _) _ = False
    typeEq _ (Left _) = False
    typeEq (Right val1) (Right val2) = typeEq val1 val2

instance TypeEq TypeNormalized TypeNormalized where
    typeEq (TypeNorm t1) (TypeNorm t2) = t1 == t2
    typeEq (LitNorm l1) (LitNorm l2)   = l1 == l2
    typeEq (CmplNorm c1) (CmplNorm c2) = typeEq c1 c2
    typeEq _ _                         = False

instance TypeEq CompilerVal Literal where
    typeEq (IntCmpl _) (IntLit _)          = True
    typeEq (DoubleCmpl _) (DoubleLit _)    = True
    typeEq (FloatCmpl _) (FloatLit _)      = True
    typeEq (BoolCmpl _) (BoolLit _)        = True
    typeEq (CharCmpl _) (CharLit _)        = True
    typeEq (ArrayCmpl _ (CharCmpl _)) (StringLit _) = True
    typeEq _ _                             = False

instance TypeEq CompilerVal Type where
    typeEq (IntCmpl _) (IntType)             = True
    typeEq (FloatCmpl _) (FloatType)         = True
    typeEq (BoolCmpl _) (BoolType)           = True
    typeEq (DoubleCmpl _) (DoubleType)       = True
    typeEq (CharCmpl _) (CharType)           = True
    typeEq (LambdaCmpl _) (LambdaType _)     = True
    typeEq (ArrayCmpl _ t1) (ArrayType (ArrayVar t2 _))   = typeEq t1 t2
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

instance TypeEq CompilerVal String where
    typeEq val str = (showType val) == str

instance ShowType CompilerVal where
    showType (IntCmpl _)       = "int"
    showType (LongCmpl _)      = "long"
    showType (DoubleCmpl _)    = "double"
    showType (FloatCmpl _)     = "float"
    showType (BoolCmpl _)      = "bool"
    showType (CharCmpl _)      = "char"
    showType (ConstCmpl _)     = "const"
    showType (LambdaCmpl _)    = "lambda"
    showType (ArrayCmpl _ val) = "array " ++ showType val
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
