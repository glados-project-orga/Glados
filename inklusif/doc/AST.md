# Inklusif Abstract Syntax Tree (AST) Documentation

This document describes the **Abstract Syntax Tree (AST)** used by the Inklusif language Parser & Compiler.
The AST is the central internal representation of programs after parsing.

---

## Overview

The AST is composed of:

* **Declarations** 
* **Statements** 
* **Expressions**
* **Types**
* **Literals**
* **Operators**
* **Source position metadata**

---

## Source Position Tracking

```haskell
data SourcePos = SourcePos
  { srcLine :: Int
  , srcColumn :: Int
  }
```

`SourcePos` stores the location of a construct in the source file. It could be used for error messages and debugging however the compiler doesn't use this data.

### Future Purpose

* Error reporting
* Debugging
* Precise diagnostics

Every top-level declaration includes a `SourcePos`.

---

## Declarations

Top-level program elements are represented by `Declaration`.

```haskell
data Declaration
  = Function FunctionDecl
  | Enum EnumDecl
  | Typedef TypedefDecl
  | Class ClassDecl
```

### Function Declaration

```haskell
data FunctionDecl = FunctionDecl
  { funcPos :: SourcePos
  , funcName :: String
  , funcParams :: [Parameter]
  , funcReturnType :: Type
  , funcBody :: [Statement]
  }
```

Represents a function with:

* Name
* Parameters
* Return type
* Body statements

---

### Class Declaration

```haskell
data ClassDecl = ClassDecl
  { classPos :: SourcePos
  , className :: String
  , classFields :: [StructField]
  , classMethods :: [FunctionDecl]
  }
```

Models object-oriented structures:

* Fields
* Methods
* No inheritance (by design)

---

### Enum Declaration

```haskell
data EnumDecl = EnumDecl
  { enumPos :: SourcePos
  , enumName :: String
  , enumDecl :: [EnumField]
  }
```

Enums define named constant values.

```haskell
data EnumField = EnumField
  { declName :: String 
  , declValue :: Maybe Int 
  }
```

If `declValue` is `Nothing`, the value is auto-incremented.

---

### Typedef Declaration

```haskell
data TypedefDecl = TypedefDecl
  { typedefPos :: SourcePos
  , typedefOriginal :: Type
  , typedefAlias :: String
  }
```

Creates a type alias without introducing a new type.

---

## Types

```haskell
data Type
  = IntType
  | FloatType
  | DoubleType
  | LongType
  | StringType
  | CharType
  | BoolType
  | VoidType
  | CustomType String
  | ArrayType ArrayVar
  | LambdaType LambdaVar
```

### Key Properties

* Strongly typed
* Explicit types everywhere
* No implicit conversion

---

### Array Types

```haskell
data ArrayVar = ArrayVar
  { arrayVarType :: Type
  , arrayVarSize :: Expr
  }
```

Array sizes are expressions, allowing dynamic sizing.

---

### Lambda Types

```haskell
data LambdaVar = LambdaVar
  { lambdaVarParamsType :: [Type]
  , lambdaVarReturnType :: Type
  , lambdaVarParamsName :: [String]
  , lambdaVarBody :: [Statement]
  }
```

Represents both:

* Lambda expressions
* Function-type variables

---

## Statements

```haskell
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
  | LambdaStatement LambdaDecl
  | Break
  | Continue
```

Statements represent **actions** rather than values.

---

### Variable Declaration

```haskell
data VarDecl = VarDecl
  { varName :: String
  , varType :: Type
  , varValue :: Expr
  , varIsConst :: Bool -- Usage of 'const' vs 'let'
  , varIsRef :: Bool
  }
```

Supports:

* `const` vs `let`
* Reference variables

---

### Control Flow Statements

* `IfStmt`
* `WhileStmt`
* `ForStmt`
* `ForEachStmt`
* `MatchStmt`
* `TryCatchStmt`

Each explicitly stores:

* Conditions
* Bodies
* Optional branches

This avoids implicit or ambiguous control flow.

---

## Expressions

```haskell
data Expr
  = LitExpr Literal
  | VarExpr String
  | ArrayVarExpr String Expr
  | ClassVarExpr String ClassAccess
  | BinOpExpr BinOp Expr Expr
  | UnaryOpExpr UnaryOp Expr
  | CallExpression CallExpr
  | MethodCallExpression MethodCallExpr
  | ClassConstructorExpr String [Expr]
  | AssignmentExpr Assignment
  | ArrayLiteral [Expr]
  | ArrayAssignement ArrayIndexExpr
  | CastExpr Type Expr
  | FieldAccessExpression FieldAccessExpr
  | StructLiteral [(String, Expr)]
  | Lambda [Parameter] [Statement]
```

Expressions **always produce a value**.

---

### Unary Expressions

```haskell
data UnaryOp
  = Neg      -- -x
  | Not      -- !x
  | PreInc   -- ++x
  | PreDec   -- --x
  | Ref      -- &x
  | Deref    -- *x
```

Unary operators apply to a single expression.

---

### Binary Expressions

```haskell
data Op
  = Add | Sub | Mul | Div | Mod
  | AddEqual | SubEqual | MulEqual | DivEqual | ModEqual
  | Equal | NotEqual
  | LessThan | GreaterThan | LessEqual | GreaterEqual
  | And | Or
```

Operators are explicitly typed and categorized.

---

## Function and Method Calls

```haskell
data CallExpr = CallExpr
  { callName :: String
  , callArgs :: [Expr]
  }
```

```haskell
data MethodCallExpr = MethodCallExpr
  { methodObj :: Expr
  , methodName :: String
  , methodArgs :: [Expr]
  }
```

This separation simplifies:

* Semantic analysis
* Method resolution
* Future optimizations

---

## Class and Field Access

```haskell
data ClassAccess
  = ClassArrayAccess String Expr
  | ClassMethodCall CallExpr
  | ClassVarAccess String
  | ClassClassAccess String ClassAccess
```

Allows chained accesses:

```ink
object.field.method().other
```

Each level is explicitly represented.

---

## Literals

```haskell
data Literal
  = IntLit Int
  | LongLit Int64
  | FloatLit Float
  | DoubleLit Double
  | StringLit String
  | CharLit Char
  | BoolLit Bool
```

Numeric types are **distinct**, avoiding ambiguity and overflow issues.

---

## Pattern Matching

```haskell
data Pattern
  = LiteralPattern Literal
  | DefaultPattern
```

```haskell
data MatchCase = MatchCase
  { matchPattern :: Pattern
  , matchBody :: Expr
  }
```

Pattern matching is expression-oriented and exhaustive by design.

---

## Design Principles

The AST follows these principles:

* Explicit over implicit
* Strong typing
* No undefined behavior
* Easy semantic analysis
* Future-proof (optimizations, codegen, VM)
