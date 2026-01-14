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
    Literal(..),
    BinOp(..),
    SourcePos(..),
    FunctionDecl(..),
    ClassDecl(..),
    EnumDecl(..),
    TypedefDecl(..),
    VarDecl(..),
    Assignment(..),
    IfStmt(..),
    WhileStmt(..),
    ForStmt(..),
    ForEachStmt(..),
    MatchStmt(..),
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

-- Position dans le parsing pour les cas d'erreurs
data SourcePos = SourcePos
  { srcLine :: Int
  , srcColumn :: Int
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
  | DoubleType
  | CharType
  | BoolType
  | ArrayType Type
  | CustomType String  -- (struct, enum, typedef)
  | VoidType -- iel
  deriving (Eq)

instance Show Type where
  show IntType = "int"
  show FloatType = "float"
  show StringType = "string"
  show CharType = "char"
  show BoolType = "bool"
  show DoubleType = "double"
  show (ArrayType t) = show t ++ "[]"
  show (CustomType s) = s
  show VoidType = "void"

-- Déclarations
data VarDecl = VarDecl
  { varName :: String
  , varType :: Type
  , varValue :: Expr
  , varIsConst :: Bool
  , varIsRef :: Bool
  } deriving (Show, Eq)

data Assignment = Assignment
  { assignTarget :: String 
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
  { arrayExpr :: Expr
  , indexExpr :: Expr
  } deriving (Show, Eq)

data FieldAccessExpr = FieldAccessExpr
  { fieldObj :: Expr
  , fieldName :: String
  } deriving (Show, Eq)

data Expr
  = LitExpr Literal
  | VarExpr String
  | BinOpExpr BinOp Expr Expr
  | UnaryOpExpr String Expr
  | CallExpression CallExpr
  | MethodCallExpression MethodCallExpr
  | AssignmentExpr Assignment
  | ArrayLiteral [Expr]
  | ArrayIndexExpression ArrayIndexExpr
  | FieldAccessExpression FieldAccessExpr
  | StructLiteral [(String, Expr)]
  | Lambda [Parameter] [Statement]
  deriving (Show, Eq)

-- je crée des valeurs litéralles parce que comme ça à la compilation on sait à 100% que c'est des const et utiliser en pattern matching
data Literal
  = IntLit Int
  | FloatLit Float
  | DoubleLit Double
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
