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
    SourcePos(..),
    FunctionDecl(..),
    StructDecl(..),
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
  , typedefAlias :: String
  , typedefOriginal :: Type
  } deriving (Show, Eq)

data Declaration
  = Function FunctionDecl
  | Struct StructDecl
  | Enum EnumDecl
  | Typedef TypedefDecl
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
  , catchType :: String
  , catchVar :: String
  , catchBody :: [Statement]
  } deriving (Show, Eq)

data ThrowStmt = ThrowStmt
  { throwType :: String
  , throwMessage :: Expr
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
  | ArrayLiteral [Expr]
  | ArrayIndexExpression ArrayIndexExpr
  | FieldAccessExpression FieldAccessExpr
  | StructLiteral String [(String, Expr)]
  | Lambda [Parameter] [Statement]
  | LoopExpr [LoopBranch] LoopResult
  deriving (Show, Eq)

-- Les loop/récursives
data LoopBranch
  = LoopCondition
      { loopCond :: Expr
      , loopResult :: LoopResult
      }
  deriving (Show, Eq)

data LoopResult
  = LoopReturn Expr 
  | LoopContinue [Statement]
  deriving (Show, Eq)

-- je crée des valeurs litéralles parce que comme ça à la compilation on sait à 100% que c'est des const et utiliser en pattern matching
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
