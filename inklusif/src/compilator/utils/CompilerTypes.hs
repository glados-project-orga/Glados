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
    Defines
) where

import Ast (
    Declaration(..),
    FunctionDecl,
    ClassDecl,
    EnumDecl,
    TypedefDecl,
    Type
    )

type Ast = [Declaration]
type ConstantPool = [String]
type Bytecode = [String]
type Defines = ([FunctionDecl], [ClassDecl], [EnumDecl], [TypedefDecl])

data SymInfo = SymInfo
  { symIndex :: Int
  , symType  :: Type
  , symConst :: Bool
  , symRef   :: Bool
  } deriving (Show, Eq)

type SymbolTable = [(String, SymInfo)]
type CompilerData = (ConstantPool, Defines, Bytecode, SymbolTable)
