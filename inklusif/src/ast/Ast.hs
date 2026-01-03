{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- Ast
-}

module Ast
(
    Declaration(..),
    Parameter(..),
    StructField(..),
    EnumField(..),
    Type(..),
    Statement(..),
    MatchCase(..),
    Pattern(..),
    Expr(..),
    LoopBranch(..),
    LoopResult(..),
    Literal(..),
    BinOp(..),
    SourcePos(..)
) where

-- Position dans le parsing pour les cas d'erreurs
data SourcePos = SourcePos
  { line :: Int
  , column :: Int
  , filename :: String
  } deriving (Show, Eq)

-- Enum
data EnumField = EnumField
  { declName :: String -- la liste d'enum "GAME", "MENU", "OPTIONS", etc..
  , declValue :: Maybe Int -- Si une value est déclaré sinon on met nothing et c'est démarré à 0
  } deriving (Show, Eq)

-- Déclarations "statement" des éléments suivants :
-- Fonction / Structure / Enum / TypeDef - Avec généralement les infos suivantes :
-- La SourcePos (en gros l'endroit ou c'est déclaré) je suis moyen sûr de ça honnêtement
-- le nom / les params / le body / etc ..
data Declaration
  = FunctionDecl
      { funcPos :: SourcePos
      , funcName :: String
      , funcParams :: [Parameter] -- PARAMETER EST DÉCLARÉ EN DESSOUS
      , funcReturnType :: Type
      , funcBody :: [Statement] -- DÉCLARÉ EN DESSOUS AUSSI
      }
  | StructDecl
      { structPos :: SourcePos
      , structName :: String
      , structFields :: [StructField] -- DESSOUS
      }
  | EnumDecl
      { enumPos :: SourcePos
      , enumName :: String
      , enumDecl :: [EnumField] -- DESSOUS
      }
  | TypedefDecl
      { typedefPos :: SourcePos
      , typedefAlias :: String
      , typedefOriginal :: Type -- DESSOUS
      }
  deriving (Show, Eq)

-- Params d'une fonction
data Parameter = Parameter
  { paramName :: String
  , paramType :: Type
  , paramIsRef :: Bool  -- Si on a une référence ou pas ?
  } deriving (Show, Eq)

-- Elems d'une structure, ça me paraît court mais je vois pas plus ?
data StructField = StructField
  { structFieldName :: String
  , structFieldType :: Type
  } deriving (Show, Eq)

-- Les types je pense c'est pas dur à expliquer
data Type
  = IntType
  | FloatType -- j'espère on le fait pas
  | StringType
  | CharType
  | BoolType
  | ArrayType Type
  | TupleType [Type]
  | LambdaType [Type] Type  -- Je crois on a aucun exemple de celui-ci donc je me dis liste de params + return type ?
  | CustomType String  -- (struct, enum, typedef)
  | VoidType -- iel
  | RefType Type
  deriving (Show, Eq)

-- Déclarations
data Statement
  = VarDecl
      { varName :: String
      , varType :: Type
      , varValue :: Expr
      , varIsConst :: Bool -- let ou const
      , varIsRef :: Bool
      }
  | Assignment
      { assignTarget :: Expr
      , assignValue :: Expr
      }
  | IfStmt
      { ifCondition :: Expr
      , ifThenBody :: [Statement]
      , ifElseBody :: Maybe [Statement]
      }
  | WhileLoop
      { whileCondition :: Expr
      , whileBody :: [Statement]
      }
  | ForLoop
      { forInit :: Maybe Statement
      , forCondition :: Expr
      , forUpdate :: [Expr]
      , forBody :: [Statement]
      }
  | ForEachLoop
      { forEachVar :: String
      , forEachCollection :: Expr
      , forEachBody :: [Statement]
      }
  | MatchStmt
      { matchExpr :: Expr
      , matchCases :: [MatchCase]
      }
  | TryCatch
      { tryBody :: [Statement]
      , catchType :: String
      , catchVar :: String
      , catchBody :: [Statement]
      }
  | ThrowStmt
      { throwType :: String
      , throwMessage :: Expr
      }
  | ReturnStmt
      { returnExpr :: Expr
      }
  | ExprStmt
      { stmtExpr :: Expr
      }
  deriving (Show, Eq)

-- Pattern matchinggg
data MatchCase
  = MatchCase
      { matchPattern :: Pattern -- DESSOUS
      , matchBody :: Expr
      }
  deriving (Show, Eq)

-- ici
data Pattern
  = LiteralPattern Literal
  | DefaultPattern  -- le _
  deriving (Show, Eq)

-- Expressions
data Expr
  = LiteralExpr Literal
  | VarExpr String
  | BinaryOp BinOp Expr Expr
  | FunctionCall
      { callName :: String
      , callArgs :: [Expr]
      }
  | MethodCall
      { methodObj :: Expr
      , methodName :: String
      , methodArgs :: [Expr]
      }
  | ArrayLiteral [Expr]
  | TupleLiteral [Expr]
  | ArrayIndex
      { arrayExpr :: Expr
      , indexExpr :: Expr
      }
  | FieldAccess
      { fieldObj :: Expr
      , fieldName :: String
      }
  | LoopExpr
      { loopBody :: [LoopBranch]
      }
  | LambdaExpr
      { lambdaParams :: [Parameter]
      , lambdaBody :: Expr
      }
  | RefExpr Expr
  deriving (Show, Eq)

-- Les loop/récursives
data LoopBranch
  = LoopCondition
      { loopCond :: Expr
      , loopResult :: LoopResult
      }
  deriving (Show, Eq)

data LoopResult
  = LoopReturn Expr  -- => expr
  | LoopContinue [Statement]
  deriving (Show, Eq)

-- je crée des valeurs litéralles parce que comme ça à la compialation on sait à 100% que c'est des const et utiliser en pattern matching
data Literal
  = IntLit Int
  | FloatLit Double
  | StringLit String
  | CharLit Char
  | BoolLit Bool
  deriving (Show, Eq)

-- ops
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  | And
  | Or
  | Xor
  deriving (Show, Eq)
