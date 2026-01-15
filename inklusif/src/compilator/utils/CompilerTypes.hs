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
) where


import Ast (
    Declaration(..),
    FunctionDecl,
    ClassDecl,
    EnumDecl,
    TypedefDecl,
    Expr,
    )

type HeapSize = Int
type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = (HeapSize, [FunctionDecl], [ClassDecl], [EnumDecl], [TypedefDecl], Int)

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

data SymInfo = SymInfo
  { symIndex :: Int
  , symVal  :: CompilerVal
  } deriving (Show, Eq)


type SymbolTable = [(String, SymInfo)]
type CompilerData = (ConstantPool, Defines, Bytecode, SymbolTable)
type CompileResult = Either String CompilerData
type CompileExpr   = Expr -> CompilerData -> CompileResult
