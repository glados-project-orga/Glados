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

---

## Coding in Inklusif

This section describes the core syntax and features of the language, with examples.

---

## Functions

### Function Declaration

```ink
fun functionName(param1 -> Type, param2 -> Type ...) : ReturnType {
    CODE
}
```

**Example:**

```ink
fun add(a -> int, b -> int) : int {
    => a + b;
}
```

---

## Conditional Statements

### `il / elle` (if / else)

```ink
il (condition) {
    CODE
} elle {
    CODE
}
```

**Example:**

```ink
il (x > 0) {
    print("positive");
} elle {
    print("negative");
}
```

---

## Loops

### While Loop

```ink
while (condition) {
    CODE
}
```

**Example:**

```ink
while (i < 10) {
    i += 1;
}
```

---

### For Loop

```ink
for (initialization; condition; iteration) {
    CODE
}
```

**Example:**

```ink
for (let i -> int = 0; i < 5; i = i + 1) {
    print(i);
}
```

---

## Pattern Matching

### Match Statement

```ink
match variable {
    expression -> result
    expression -> result
    ...
    _ -> defaultResult
}
```

**Example:**

```ink
match x {
    0 -> print("zero");
    1 -> print("one");
    _ -> print("other");
}
```

---

## Variables

### Variable Declaration

#### Constant Value

```ink
const name -> Type = expression;
```

#### Mutable Value

```ink
let name -> Type = expression;
```

**Example:**

```ink
const max -> int = 100;
let counter -> int = 0;
```

---

### Assignment

```ink
variable = expression;
```

**Examples:**

```ink
nb = nb + 10;
arr[0] = 42;
class.field = "value";
```

---

## Lambda Expressions

Lambdas are fully typed and can be assigned to variables or passed as parameters.

```ink
let lambdaName -> (ArgTypes) => ReturnType =
    [argNames] {
        CODE
    };
```

**Example:**

```ink
let sum -> (int, int) => int =
    [a, b] {
        => a + b;
    };
```

Parameter names are optional but "()" & "[]" are mandatory :

```ink
let constant -> () -> int =
    [] {
        => 42;
    };
```

---

## Enums

### Enum Declaration

```ink
enum EnumName {
    VALUE = 10,
    OTHER,
    ANOTHER
}
```

Values without an explicit assignment are automatically incremented.

**Example:**

```ink
enum ErrorCode {
    OK = 0,
    NOtOK,
    IDKwhyIMhere
}
```

---

## Typedefs

Typedefs allow you to create aliases for existing types.

```ink
typedef OriginalType NewType;
```

**Example:**

```ink
typedef int Integer;
```

---

## Classes

### Class Declaration

```ink
class ClassName {
    fieldName -> Type;
    ...

    method methodName(params) : ReturnType {
        CODE
    }
}
```

**Example:**

```ink
class Point {
    x -> int;
    y -> int;

    method move(dx -> int, dy -> int) : void {
        this.x = this.x + dx;
        this.y = this.y + dy;
    }
}
```

---

### Instantiating a Class

```ink
let variable -> ClassName = new ClassName();
```

**Example:**

```ink
let p -> Point = new Point();
```

---

## Summary

Inklusif combines:

* Strong typing
* Functional concepts (lambdas, pattern matching)
* Familiar imperative constructs (loops, classes)

while remaining simple and expressive.

---

### Students

| Celian Raguin | Compilator / Interpreter |
| Kerwan Calvier | VM & Compilator |
| Aurel Pliya | VM & Compilator |
| Hajar Ahazzam | Compilator / Interpreter |
| Luigi Gomes | Parser, AST, Documentation |

_Epitech_

