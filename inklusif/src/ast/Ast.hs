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
    ArrayVar(..),
    Statement(..),
    MatchCase(..),
    UnaryOp(..),
    Pattern(..),
    Expr(..),
    Literal(..),
    BinOp(..),
    SourcePos(..),
    FunctionDecl(..),
    ClassDecl(..),
    LambdaVar(..),
    EnumDecl(..),
    TypedefDecl(..),
    VarDecl(..),
    Assignment(..),
    IfStmt(..),
    WhileStmt(..),
    ForStmt(..),
    ForEachStmt(..),
    MatchStmt(..),
    LambdaDecl(..),
    TryCatchStmt(..),
    ThrowStmt(..),
    ReturnStmt(..),
    ExprStmt(..),
    CallExpr(..),
    MethodCallExpr(..),
    ArrayIndexExpr(..),
    FieldAccessExpr(..),
    ForUpdate(..)
) where

import Data.Int (Int64)

-- Position dans le parsing pour les cas d'erreurs
data SourcePos = SourcePos
  { srcLine :: Int
  , srcColumn :: Int
  } deriving (Show, Eq)

-- Enum
data EnumField = EnumField
  { declName :: String
  , declValue :: Maybe Int
  } deriving (Show, Eq)

-- Déclarations "statement" des éléments suivants :
-- Fonction / Structure / Enum / TypeDef - Avec généralement les infos suivantes :
-- La SourcePos (en gros l'endroit ou c'est déclaré)
-- le nom / les params / le body / etc ..

data FunctionDecl = FunctionDecl
  { funcPos :: SourcePos
  , funcName :: String
  , funcParams :: [Parameter]
  , funcReturnType :: Type
  , funcBody :: [Statement]
  } deriving (Show, Eq)

data StructDecl = StructDecl
  { structPos :: SourcePos
  , structName :: String
  , structFields :: [StructField]
  } deriving (Show, Eq)

data EnumDecl = EnumDecl
  { enumPos :: SourcePos
  , enumName :: String
  , enumDecl :: [EnumField]
  } deriving (Show, Eq)

data TypedefDecl = TypedefDecl
  { typedefPos :: SourcePos
  , typedefOriginal :: Type
  , typedefAlias :: String
  } deriving (Show, Eq)

data ClassDecl = ClassDecl
  { classPos :: SourcePos
  , className :: String
  , classFields :: [StructField]
  , classMethods :: [FunctionDecl]
  } deriving (Show, Eq)

data Declaration
  = Function FunctionDecl
  | Enum EnumDecl
  | Typedef TypedefDecl
  | Class ClassDecl
  deriving (Show, Eq)

data Parameter = Parameter
  { paramName :: String
  , paramType :: Type
  , paramIsRef :: Bool
  } deriving (Show, Eq)

data StructField = StructField
  { structFieldName :: String
  , structFieldType :: Type
  } deriving (Show, Eq)

data ArrayVar = ArrayVar
  { arrayVarType :: Type
  , arrayVarSize :: Expr
  } deriving (Show, Eq)

data LambdaVar = LambdaVar
  { lambdaVarParamsType :: [Type]
  , lambdaVarReturnType :: Type
  , lambdaVarParamsName :: [String]
  , lambdaVarBody :: [Statement]
  } deriving (Show, Eq)

data Type
  = IntType
  | FloatType
  | StringType
  | DoubleType
  | CharType
  | LongType
  | BoolType
  | LambdaType LambdaVar
  | ArrayType ArrayVar
  | CustomType String  -- (struct, enum, typedef)
  | VoidType -- iel
  deriving (Eq)

instance Show Type where
  show IntType = "int"
  show FloatType = "float"
  show StringType = "string"
  show CharType = "char"
  show BoolType = "bool"
  show LongType = "long"
  show (LambdaType _) = "lambda"
  show DoubleType = "double"
  show (ArrayType (ArrayVar t _)) = "array " ++ show t
  show (CustomType s) = s
  show VoidType = "void"

data LambdaDecl = LambdaDecl
  { lambdaName :: String
  , lambdaContent :: LambdaVar
  } deriving (Show, Eq)

data VarDecl = VarDecl
  { varName :: String
  , varType :: Type
  , varValue :: Expr
  , varIsConst :: Bool
  , varIsRef :: Bool
  } deriving (Show, Eq)

data Assignment = Assignment
  { assignTarget :: Expr 
  , assignValue :: Expr
  } deriving (Show, Eq)

data IfStmt = IfStmt
  { ifCondition :: Expr
  , ifThenBody :: [Statement]
  , ifElseBody :: Maybe [Statement]
  } deriving (Show, Eq)

data WhileStmt = WhileStmt
  { whileCondition :: Expr
  , whileBody :: [Statement]
  } deriving (Show, Eq)


data ForUpdate
    = ForUpdateExpr Expr
    | ForUpdateStmt Statement
    deriving (Show, Eq)

data ForStmt = ForStmt
  { forInit :: Maybe Statement
  , forCondition :: Expr
  , forUpdate :: ForUpdate 
  , forBody :: [Statement]
  } deriving (Show, Eq)

data ForEachStmt = ForEachStmt
  { forEachVar :: String
  , forEachCollection :: Expr
  , forEachBody :: [Statement]
  } deriving (Show, Eq)

data MatchStmt = MatchStmt
  { matchExpr :: Expr
  , matchCases :: [MatchCase]
  } deriving (Show, Eq)

data TryCatchStmt = TryCatchStmt
  { tryBody :: [Statement]
  , catchVar :: Maybe String
  , catchBody :: [Statement]
  } deriving (Show, Eq)

data ThrowStmt = ThrowStmt
  { throwMessage :: Expr
  } deriving (Show, Eq)

data ReturnStmt = ReturnStmt
  { returnExpr :: Expr
  } deriving (Show, Eq)

data ExprStmt = ExprStmt
  { stmtExpr :: Expr
  } deriving (Show, Eq)

data Statement
  = VarDeclStmt VarDecl
  | AssignmentStmt Assignment
  | IfStatement IfStmt
  | LambdaStatement LambdaDecl
  | WhileStatement WhileStmt
  | ForStatement ForStmt
  | ForEachStatement ForEachStmt
  | MatchStatement MatchStmt
  | TryCatchStatement TryCatchStmt
  | ThrowStatement ThrowStmt
  | ReturnStatement ReturnStmt
  | ExprStatement ExprStmt
  | Break
  | Continue
  deriving (Show, Eq)

data MatchCase
  = MatchCase
      { matchPattern :: Pattern
      , matchBody :: Expr
      }
  deriving (Show, Eq)

data Pattern
  = LiteralPattern Literal
  | DefaultPattern  -- le _
  deriving (Show, Eq)

data CallExpr = CallExpr
  { callName :: String
  , callArgs :: [Expr]
  } deriving (Show, Eq)

data MethodCallExpr = MethodCallExpr
  { methodObj :: Expr
  , methodName :: String
  , methodArgs :: [Expr]
  } deriving (Show, Eq)

data ArrayIndexExpr = ArrayIndexExpr
  { arrayVar :: String
  , indexArrayExpr :: Expr
  , arrayValueExpr :: Expr
  } deriving (Show, Eq)

data FieldAccessExpr = FieldAccessExpr
  { fieldObj :: Expr
  , fieldName :: String
  } deriving (Show, Eq)

data Expr
  = LitExpr Literal
  | VarExpr String
  | ArrayVarExpr String Expr
  | ClassVarExpr String Expr
  | BinOpExpr BinOp Expr Expr
  | UnaryOpExpr UnaryOp Expr
  | CallExpression CallExpr
  | ClassConstructorExpr String [Expr]
  | MethodCallExpression MethodCallExpr
  | AssignmentExpr Assignment
  | ArrayLiteral [Expr]
  | ArrayAssignement ArrayIndexExpr
  | CastExpr Type Expr
  | FieldAccessExpression FieldAccessExpr
  | StructLiteral [(String, Expr)]
  | Lambda [Parameter] [Statement]
  deriving (Show, Eq)

data UnaryOp
  = Neg        -- -x
  | Not        -- !x
  | PreInc     -- ++x
  | PreDec     -- --x
  | Ref        -- &x
  | Deref      -- *x
  deriving (Show, Eq)

data Literal
  = IntLit Int
  | FloatLit Float
  | DoubleLit Double
  | StringLit String
  | LongLit Int64
  | CharLit Char
  | BoolLit Bool
  deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | AddEqual
  | SubEqual
  | MulEqual
  | DivEqual
  | ModEqual
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  | And
  | Or
  deriving (Show, Eq)
