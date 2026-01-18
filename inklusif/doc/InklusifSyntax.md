# Inklusif

**Inklusif** is a small **functional-style programming language** developed as part of an **Epitech project** by a group of five students.

The language supports:

* Pattern matching
* Loops (`while`, `for`)
* Lambda expressions
* References
* User-defined types (`enum`, `typedef`, `class`)
* Basic data structures

Every Inklusif source file must use the **`.ink`** extension.

# Lexical Expressions

**identifier:**

![identifier](diagram/identifier.png)

```
identifier
         ::= letter ( letter | digit | '_' )*
```

**integer:**

![integer](diagram/integer.png)

```
integer  ::= digit+
```

**float:**

![float](diagram/float.png)

```
float    ::= digit+ '.' digit+
```

**string:**

![string](diagram//string.png)

```
string   ::= '"' any_char_except_quote '"'
```

**char:**

![char](diagram/char.png)

```
char     ::= "'" any_char "'"
```

## Program Content

**Program Structure:**

![Program](diagram/InkFile.png)

```
InkFile  ::= Declaration*
```

## Declaration

![Declaration](diagram/Declaration.png)

```
Declaration
         ::= FunctionDecl
           | EnumDecl
           | StructDecl
           | ClassDecl
           | TypedefDecl
```

## Function Declaration

**FunctionDecl**

![FunDecl](diagram/FunctionDecl.png)

```
FunctionDecl
         ::= 'fun' Identifier '(' ParamList? ')' ':' Type Block
```

**ParamList**

![Params](diagram/ParamList.png)

```
ParamList
         ::= Parameter ( ',' Parameter )*
```

**Parameter**

![Param](diagram/Parameter.png)

```
Parameter
         ::= '&'? Identifier '->' Type
```

**Block**

![Block](diagram/Block.png)

```
Block    ::= '{' Statement* '}'
```

**Statement**

![Statement](diagram/Statement.png)

```
Statement
         ::= VarDecl
           | AssignmentStmt
           | ExprStmt
           | IfStmt
           | WhileStmt
           | ForStmt
           | MatchStmt
           | ReturnStmt
           | TryCatchStmt
```

**VarDecl**

![VarDecl](diagram/VarDecl.png)

```
VarDecl  ::= ( 'let' | 'const' ) Identifier '->' Type '=' Expression ';'
```

**AssignmentStmt**

![AssignmentStmt](diagram/AssignmentStmt.png)

```
AssignmentStmt
         ::= AssignTarget AssignOp Expression ';'
```

**AssignTarget**

![AssignTarget](diagram/AssignTarget.png)

```
AssignTarget
         ::= Identifier ( '[' Expression ']' | '.' Identifier )?
```

**AssignOp**

![AssignOp](diagram/AssignOp.png)

```
AssignOp ::= '='
           | '+='
           | '-='
           | '*='
           | '/='
           | '%='
```

**ExprStmt**

![ExprStmt](diagram/ExprStmt.png)

```
ExprStmt ::= Expression ';'
```

**Expression**

![Expression](diagram/Expression.png)

```
Expression
         ::= ComparisonExpr
```

**ComparisonExpr**

![ComparisonExpr](diagram/ComparisonExpr.png)

```
ComparisonExpr
         ::= AdditiveExpr ( CompOp AdditiveExpr )*
```

**CompOp**

![CompOp](diagram/CompOp.png)

```
CompOp   ::= '=='
           | '!='
           | '<'
           | '>'
           | '<='
           | '>='
```

**AdditiveExpr**

![AdditiveExpr](diagram/AdditiveExpr.png)

```
AdditiveExpr
         ::= MultiplicativeExpr ( AddOp MultiplicativeExpr )*
```

**AddOp**

![AddOp](diagram/AddOp.png)

```
AddOp    ::= '+'
           | '-'
```

**MultiplicativeExpr**

![MultiplicativeExpr](diagram/MultiplicativeExpr.png)

```
MultiplicativeExpr
         ::= UnaryExpr ( MulOp UnaryExpr )*
```

**MulOp**

![MulOp](diagram/MulOp.png)

```
MulOp    ::= '*'
           | '/'
           | '%'
```

**UnaryExpr**

![UnaryExpr](diagram/UnaryExpr.png)

```
UnaryExpr
         ::= UnaryOp* PostfixExpr
```

**UnaryOp**

![UnaryOp](diagram/UnaryOp.png)

```
UnaryOp  ::= '-'
           | '!'
           | '++'
           | '--'
           | '&'
           | '*'
```

**PostfixExpr**

![PostfixExpr](diagram/PostfixExpr.png)

```
PostfixExpr
         ::= PrimaryExpr PostfixOp*
```

**PostfixOp**


![PostfixOp](diagram/PostfixOp.png)

```
PostfixOp
         ::= '++'
           | '--'
           | '(' ArgumentList? ')'
           | '[' Expression ']'
           | '.' Identifier
```

**ArgumentList**

![ArgumentList](diagram/ArgumentList.png)

```
ArgumentList
         ::= Expression ( ',' Expression )*
```

**PrimaryExpr**

![PrimaryExpr](diagram/PrimaryExpr.png)

```
PrimaryExpr
         ::= Literal
           | Identifier
           | '(' Expression ')'
           | ArrayLiteral
           | StructLiteral
           | LambdaExpr
           | ClassConstructor
```

**Literal**

![Literal](diagram/Literal.png)

```
Literal  ::= IntegerLiteral
           | LongLiteral
           | FloatLiteral
           | DoubleLiteral
           | StringLiteral
           | CharLiteral
           | BooleanLiteral
```

**BooleanLiteral**

![BooleanLiteral](diagram/BooleanLiteral.png)

```
BooleanLiteral
         ::= 'true'
           | 'false'
```

**ArrayLiteral**

![ArrayLiteral](diagram/ArrayLiteral.png)

```
ArrayLiteral
         ::= '[' ( Expression ( ',' Expression )* )? ']'
```

**ifStmt**

![IfStmt](diagram/IfStmt.png)

```
IfStmt 
        ::= 'il' '(' Expression ')' Block ( 'elle' Block )?
```

**WhileStmt**

![WhileStmt](diagram/WhileStmt.png)

```
WhileStmt
         ::= 'while' '(' Expression ')' Block
```

**ForStmt**

![ForStmt](diagram/ForStmt.png)

```
ForStmt  ::= 'for' '(' VarDecl? ';' Expression? ';' Expression? ')' Block
```

**MatchStmt**

![MatchStmt](diagram/MatchStmt.png)

```
MatchStmt
         ::= 'match' Expression '{' MatchCase+ '}'
```

**MatchCase**

![MatchCase](diagram/MatchCase.png)

```
MatchCase
         ::= ( Expression | '_' ) '->' Expression ';'
```

**ReturnStmt**

![ReturnStmt](diagram/ReturnStmt.png)

```
ReturnStmt
         ::= '=>' Expression ';'
```

**TryCatchStmt**

![TryCatchStmt](diagram/TryCatchStmt.png)

```
TryCatchStmt
         ::= 'try' Block 'catch' '(' Identifier ')' Block
```

**Type**

![Type](diagram/Type.png)

```
Type     ::= PrimitiveType
           | LambdaType
           | ArrayType
           | CustomType
```

**Primitive Type**

![PrimType](diagram/PrimitiveType.png)

```
PrimitiveType
         ::= 'int'
           | 'float'
           | 'double'
           | 'string'
           | 'char'
           | 'bool'
           | 'void'
```

**LambdaType**

![Lambda](diagram/LambdaType.png)

```
LambdaType
         ::= '(' TypeList? ')' '=>' Type
```

**Array type** 

![Array](diagram/ArrayType.png)

```
ArrayType
         ::= Type '[' Expression ']'
```

**Custom Type** 

![Custom](diagram/CustomType.png)

```
CustomType
         ::= Identifier
```

## Enum Declaration

**EnumDecl**

![EnumDecl](diagram/EnumDecl.png)

```
EnumDecl ::= 'enum' Identifier '{' EnumField ( ',' EnumField )* '}'
```

**EnumField**

![EnumField](diagram/EnumField.png)

```
EnumField
         ::= Identifier ( '=' IntegerLiteral )?
```

## Class Declaration

**ClassDecl**

![ClassDecl](diagram/ClassDecl.png)

```
ClassDecl
         ::= 'class' Identifier '{' ClassMember* '}'
```

**Class Member**

![ClassMember](diagram/ClassMember.png)

```
ClassMember
         ::= StructField
           | MethodDecl
```

**Struct Field**

![Struct Field](diagram/StructField.png)

```
StructField
         ::= Identifier '->' Type ';'
```

**MethodDecl**

![Method Declaration](diagram/MethodDecl.png)

```
MethodDecl
         ::= 'method' Identifier '(' ParamList? ')' ':' Type Block
```

## TypeDef Declaration

**TypedefDecl**

![TypeDef](diagram/TypedefDecl.png)

```
TypedefDecl
         ::= 'typedef' Type Identifier ';'
```