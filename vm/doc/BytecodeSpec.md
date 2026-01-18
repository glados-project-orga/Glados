# GLaDOS Virtual Machine Bytecode Specification

This document describes the bytecode instruction set, file format, and execution model for the GLaDOS Virtual Machine. It serves as a reference for both the Inklusif compiler and the VM implementation.

---

## Table of Contents

1. [Overview](#overview)
2. [File Format](#file-format)
3. [Data Types](#data-types)
4. [Instruction Reference](#instruction-reference)
   - [Constants](#constants)
   - [Load & Store](#load--store)
   - [Arithmetic](#arithmetic)
   - [Bitwise Operations](#bitwise-operations)
   - [Type Conversions](#type-conversions)
   - [Stack Manipulation](#stack-manipulation)
   - [Comparisons](#comparisons)
   - [Control Flow](#control-flow)
   - [Function Calls](#function-calls)
   - [Arrays](#arrays)
   - [Objects](#objects)
   - [I/O](#io)
5. [Runtime Safety](#runtime-safety)
6. [Compilation Process](#compilation-process)
7. [Examples](#examples)

---

## Overview

The GLaDOS VM is a **stack-based virtual machine** that executes bytecode compiled from the Inklusif programming language. Key characteristics:

- **Stack-based architecture**: Operations push/pop values from an operand stack
- **Typed values**: All values have explicit types (int, long, float, double, char, reference)
- **Function-oriented**: Code is organized into named functions
- **Absolute jumps**: All jump targets are absolute instruction indices (not offsets)
- **Strong runtime checks**: Division by zero, stack underflow, type mismatches, and bounds checking

### Execution Model

```
+------------------+
|   Constant Pool  |  <- Compile-time constants
+------------------+
|    Functions     |  <- Named code blocks
+------------------+

Per-function execution:
+------------------+
|  Operand Stack   |  <- Values for operations
+------------------+
|  Local Variables |  <- Function parameters and locals
+------------------+
|   Call Frames    |  <- Saved state for function calls
+------------------+
```

---

## File Format

### Text Format (Human-Readable)

Bytecode files use a human-readable text format for easy debugging and inspection.

#### Structure

```
header {
    <constant1>;
    <constant2>;
    ...
}

fun <function_name> {
    <instruction>
    <instruction>
    ...
}

fun <function_name> {
    ...
}
```

#### Header Section

The header contains the **constant pool** - a list of compile-time constants that can be referenced by `ldc` instructions.

```
header {
    42;              ; Integer constant (index 0)
    3.14f;           ; Float constant (index 1)
    2.71828;         ; Double constant (index 2)
    999999999L;      ; Long constant (index 3)
    "hello";         ; String constant (index 4)
    'x';             ; Char constant (index 5)
    true;            ; Boolean constant (index 6)
}
```

**Type Suffixes:**
| Suffix | Type | Example |
|--------|------|---------|
| (none) | Integer (32-bit) | `42` |
| `L` or `l` | Long (64-bit) | `999999999L` |
| `f` or `F` | Float (32-bit) | `3.14f` |
| (decimal, no suffix) | Double (64-bit) | `2.71828` |

#### Function Section

Functions are defined with the `fun` keyword:

```
fun main {
    iconst 5
    iconst 3
    iadd
    ireturn
}
```

**Entry Point**: Execution begins at the function named `main`.

---

## Data Types

### Value Types

| Type | Size | Description | Example |
|------|------|-------------|---------|
| `int` | 32-bit | Signed integer | `42`, `-1` |
| `long` | 64-bit | Signed long integer | `9999999999L` |
| `float` | 32-bit | single precision | `3.14f` |
| `double` | 64-bit | double precision | `2.71828` |
| `char` | 8-bit | ASCII character | `'a'` |
| `bool` | - | Boolean (stored as int 0/1) | `true`, `false` |
| `reference` | - | Heap object/array handle | - |
| `null` | - | Null reference | - |

### Type Prefixes in Instructions

| Prefix | Type |
|--------|------|
| `i` | int (32-bit integer) |
| `l` | long (64-bit integer) |
| `f` | float (32-bit float) |
| `d` | double (64-bit float) |
| `a` | reference (address/pointer) |
| `c` | char |

---

## Instruction Reference

### Constants

Push constant values onto the stack.

| Instruction | Operand | Description | Stack Effect |
|-------------|---------|-------------|--------------|
| `iconst <n>` | int | Push integer constant | ... -> ..., int |
| `lconst <n>` | long | Push long constant | ... -> ..., long |
| `fconst <n>` | float | Push float constant | ... -> ..., float |
| `dconst <n>` | double | Push double constant | ... -> ..., double |
| `cconst <c>` | char | Push char constant | ... -> ..., char |
| `sconst <s>` | string | Push string constant | ... -> ..., string |
| `ldc <idx>` | index | Load from constant pool | ... -> ..., value |

**Examples:**
```
iconst 42        ; Push integer 42
lconst 100       ; Push long 100
fconst 3.14      ; Push float 3.14
dconst 2.718     ; Push double 2.718
cconst 'A'       ; Push char 'A'
sconst "hello"   ; Push string "hello"
ldc 0            ; Push constant_pool[0]
```

---

### Load & Store

Transfer values between the operand stack and local variables.

#### Integer (32-bit)

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `iload <n>` | index | Load int from local variable n |
| `istore <n>` | index | Store int to local variable n |

#### Long (64-bit)

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `lload <n>` | index | Load long from local variable n |
| `lstore <n>` | index | Store long to local variable n |

#### Float (32-bit)

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `fload <n>` | index | Load float from local variable n |
| `fstore <n>` | index | Store float to local variable n |

#### Double (64-bit)

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `dload <n>` | index | Load double from local variable n |
| `dstore <n>` | index | Store double to local variable n |

#### Char

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `cload <n>` | index | Load char from local variable n |
| `cstore <n>` | index | Store char to local variable n |

#### Reference

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `aload <n>` | index | Load reference from local variable n |
| `astore <n>` | index | Store reference to local variable n |

#### Increment

| Instruction | Operands | Description |
|-------------|----------|-------------|
| `iinc <n> <k>` | index, const | Increment local variable n by constant k |

**Examples:**
```
iload 0          ; Load int from local[0]
istore 1         ; Store int to local[1]
iinc 0 1         ; Increment local[0] by 1 (i++)
iinc 2 -1        ; Decrement local[2] by 1 (i--)
```

---

### Arithmetic

#### Integer Arithmetic

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `iadd` | Integer addition | ..., a, b -> ..., (a + b) |
| `isub` | Integer subtraction | ..., a, b -> ..., (a - b) |
| `imul` | Integer multiplication | ..., a, b -> ..., (a * b) |
| `idiv` | Integer division | ..., a, b -> ..., (a / b) |
| `irem` | Integer remainder | ..., a, b -> ..., (a % b) |
| `ineg` | Integer negation | ..., a -> ..., (-a) |

#### Long Arithmetic

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `ladd` | Long addition | ..., a, b -> ..., (a + b) |
| `lsub` | Long subtraction | ..., a, b -> ..., (a - b) |
| `lmul` | Long multiplication | ..., a, b -> ..., (a * b) |
| `ldiv` | Long division | ..., a, b -> ..., (a / b) |
| `lrem` | Long remainder | ..., a, b -> ..., (a % b) |
| `lneg` | Long negation | ..., a -> ..., (-a) |

#### Float Arithmetic

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `fadd` | Float addition | ..., a, b -> ..., (a + b) |
| `fsub` | Float subtraction | ..., a, b -> ..., (a - b) |
| `fmul` | Float multiplication | ..., a, b -> ..., (a * b) |
| `fdiv` | Float division | ..., a, b -> ..., (a / b) |
| `frem` | Float remainder | ..., a, b -> ..., (a % b) |
| `fneg` | Float negation | ..., a -> ..., (-a) |

#### Double Arithmetic

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `dadd` | Double addition | ..., a, b -> ..., (a + b) |
| `dsub` | Double subtraction | ..., a, b -> ..., (a - b) |
| `dmul` | Double multiplication | ..., a, b -> ..., (a * b) |
| `ddiv` | Double division | ..., a, b -> ..., (a / b) |
| `drem` | Double remainder | ..., a, b -> ..., (a % b) |
| `dneg` | Double negation | ..., a -> ..., (-a) |

**Example - Calculate (a + b) * c:**
```
iload 0          ; Load a
iload 1          ; Load b
iadd             ; a + b
iload 2          ; Load c
imul             ; (a + b) * c
```

---

### Bitwise Operations

#### Integer Bitwise

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `iand` | Bitwise AND | ..., a, b -> ..., (a & b) |
| `ior` | Bitwise OR | ..., a, b -> ..., (a \| b) |
| `ixor` | Bitwise XOR | ..., a, b -> ..., (a ^ b) |
| `ishl` | Shift left | ..., a, n -> ..., (a << n) |
| `ishr` | Shift right (signed) | ..., a, n -> ..., (a >> n) |

#### Long Bitwise

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `land` | Bitwise AND | ..., a, b -> ..., (a & b) |
| `lor` | Bitwise OR | ..., a, b -> ..., (a \| b) |
| `lxor` | Bitwise XOR | ..., a, b -> ..., (a ^ b) |
| `lshl` | Shift left | ..., a, n -> ..., (a << n) |
| `lshr` | Shift right (signed) | ..., a, n -> ..., (a >> n) |

**Note:** Shift amounts are masked to prevent excessive shifts:
- Integer shifts: `n & 0x1F` (0-31 bits)
- Long shifts: `n & 0x3F` (0-63 bits)

---

### Type Conversions

Convert between numeric types.

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `i2l` | Int to long | ..., int -> ..., long |
| `i2f` | Int to float | ..., int -> ..., float |
| `i2d` | Int to double | ..., int -> ..., double |
| `i2c` | Int to char | ..., int -> ..., char |
| `l2i` | Long to int | ..., long -> ..., int |
| `l2f` | Long to float | ..., long -> ..., float |
| `l2d` | Long to double | ..., long -> ..., double |
| `f2i` | Float to int | ..., float -> ..., int |
| `f2l` | Float to long | ..., float -> ..., long |
| `f2d` | Float to double | ..., float -> ..., double |
| `d2i` | Double to int | ..., double -> ..., int |
| `d2l` | Double to long | ..., double -> ..., long |
| `d2f` | Double to float | ..., double -> ..., float |
| `c2i` | Char to int | ..., char -> ..., int |

**Example - Convert int to float for division:**
```
iload 0          ; Load integer
i2f              ; Convert to float
fconst 2.0       ; Push 2.0f
fdiv             ; Floating-point division
```

---

### Stack Manipulation

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `nop` | No operation | ... -> ... |
| `pop` | Pop top value | ..., v -> ... |
| `pop2` | Pop two values (or one 64-bit) | ..., v2, v1 -> ... |
| `dup` | Duplicate top value | ..., v -> ..., v, v |
| `dup2` | Duplicate top two values | ..., v2, v1 -> ..., v2, v1, v2, v1 |
| `dup_x1` | Dup top, insert below second | ..., v2, v1 -> ..., v1, v2, v1 |
| `dup_x2` | Dup top, insert below third | ..., v3, v2, v1 -> ..., v1, v3, v2, v1 |
| `dup2_x1` | Dup two, insert below third | ..., v3, v2, v1 -> ..., v2, v1, v3, v2, v1 |
| `dup2_x2` | Dup two, insert below fourth | ..., v4, v3, v2, v1 -> ..., v2, v1, v4, v3, v2, v1 |
| `swap` | Swap top two values | ..., v2, v1 -> ..., v1, v2 |

**Example - Square a number (n * n):**
```
iload 0          ; Load n
dup              ; Duplicate: [n, n]
imul             ; n * n
```

---

### Comparisons

#### Compare with Zero (Conditional Branch)

| Instruction | Operand | Description | Condition |
|-------------|---------|-------------|-----------|
| `ifeq <target>` | address | Branch if == 0 | value == 0 |
| `ifne <target>` | address | Branch if != 0 | value != 0 |
| `iflt <target>` | address | Branch if < 0 | value < 0 |
| `ifge <target>` | address | Branch if >= 0 | value >= 0 |
| `ifgt <target>` | address | Branch if > 0 | value > 0 |
| `ifle <target>` | address | Branch if <= 0 | value <= 0 |

#### Compare Two Integers (Conditional Branch)

| Instruction | Operand | Description | Condition |
|-------------|---------|-------------|-----------|
| `if_icmpeq <target>` | address | Branch if a == b | a == b |
| `if_icmpne <target>` | address | Branch if a != b | a != b |
| `if_icmplt <target>` | address | Branch if a < b | a < b |
| `if_icmpge <target>` | address | Branch if a >= b | a >= b |
| `if_icmpgt <target>` | address | Branch if a > b | a > b |
| `if_icmple <target>` | address | Branch if a <= b | a <= b |

#### Compare Two References

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `if_acmpeq <target>` | address | Branch if references are equal |
| `if_acmpne <target>` | address | Branch if references are not equal |

#### Numeric Comparisons (Push Result)

| Instruction | Description | Result |
|-------------|-------------|--------|
| `lcmp` | Compare two longs | -1 if a < b, 0 if a == b, 1 if a > b |
| `fcmpl` | Compare floats (NaN -> -1) | -1, 0, or 1 |
| `fcmpg` | Compare floats (NaN -> 1) | -1, 0, or 1 |
| `dcmpl` | Compare doubles (NaN -> -1) | -1, 0, or 1 |
| `dcmpg` | Compare doubles (NaN -> 1) | -1, 0, or 1 |

**Note:** Jump targets are **absolute instruction indices**, not offsets.

**Example - If-else:**
```
; if (x > 0) { return 1; } else { return 0; }
    iload 0          ; Load x
    ifle 4           ; If x <= 0, jump to instruction 4
    iconst 1         ; Push 1
    ireturn          ; Return 1
    iconst 0         ; Push 0 (instruction 4)
    ireturn          ; Return 0
```

---

### Control Flow

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `goto <target>` | address | Unconditional jump to target |

**Note:** The target is an **absolute instruction index** within the current function.

**Example - While loop:**
```
; while (i < n) { i++; }
; locals: i = 0, n = 1
fun loop_example {
    iload 0          ; 0: Load i
    iload 1          ; 1: Load n
    if_icmpge 5      ; 2: If i >= n, exit loop (jump to 5)
    iinc 0 1         ; 3: i++
    goto 0           ; 4: Jump back to start
    return           ; 5: Exit
}
```

---

### Function Calls

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `invokestatic <name>` | function name | Call a static function |

#### Return Instructions

| Instruction | Description | Stack: Before -> After |
|-------------|-------------|------------------------|
| `return` | Return void | ... -> |
| `ireturn` | Return int | ..., int -> |
| `lreturn` | Return long | ..., long -> |
| `freturn` | Return float | ..., float -> |
| `dreturn` | Return double | ..., double -> |
| `creturn` | Return char | ..., char -> |
| `areturn` | Return reference | ..., ref -> |

**Calling Convention:**
1. Push arguments onto the stack (left to right)
2. Call `invokestatic <function>`
3. Arguments are copied to callee's local variables (0, 1, 2, ...)
4. Return value (if any) is pushed onto caller's stack

**Example:**
```
; Call add(3, 5)
fun main {
    iconst 3         ; Push first argument
    iconst 5         ; Push second argument
    invokestatic add ; Call add
    ireturn          ; Return result
}

fun add {
    iload 0          ; Load first argument (local 0)
    iload 1          ; Load second argument (local 1)
    iadd             ; Add them
    ireturn          ; Return result
}
```

---

### Arrays

| Instruction | Description | Stack Effect |
|-------------|-------------|--------------|
| `newarray` | Create new array | ..., size -> ..., arrayref |
| `iaload` | Load from array | ..., arrayref, index -> ..., value |
| `iastore` | Store to array | ..., arrayref, index, value -> ... |
| `arraylength` | Get array length | ..., arrayref -> ..., length |

**Example - Create and fill array:**
```
; Create array of size 3 and set arr[0] = 42
    iconst 3         ; Size
    newarray         ; Create array
    astore 0         ; Store reference in local 0
    aload 0          ; Load array reference
    iconst 0         ; Index 0
    iconst 42        ; Value 42
    iastore          ; arr[0] = 42
```

---

### Objects

| Instruction | Operand | Description | Stack Effect |
|-------------|---------|-------------|--------------|
| `new <class>` | class name | Create new object | ... -> ..., objectref |
| `getfield <field>` | field name | Get field value | ..., objectref -> ..., value |
| `putfield <field>` | field name | Set field value | ..., objectref, value -> ... |

**Example:**
```
    new Point        ; Create Point object
    astore 0         ; Store reference
    aload 0          ; Load reference
    iconst 10        ; Value for x
    putfield x       ; point.x = 10
    aload 0          ; Load reference
    getfield x       ; Get point.x
```

---

### I/O

| Instruction | Operand | Description |
|-------------|---------|-------------|
| `invoke_write <n>` | count | Print n values from stack |

**Example:**
```
    iconst 42        ; Push value to print
    invoke_write 1   ; Print 1 value
```

---

## Runtime Safety

The VM implements comprehensive runtime safety checks:

### Division by Zero

All division and remainder operations check for zero divisor:
- `idiv`, `irem` -> "idiv: division by zero", "irem: division by zero"
- `ldiv`, `lrem` -> "ldiv: division by zero", "lrem: division by zero"
- `fdiv`, `frem` -> "fdiv: division by zero", "frem: division by zero"
- `ddiv`, `drem` -> "ddiv: division by zero", "drem: division by zero"

### Stack Safety

- **Stack underflow**: All operations verify sufficient stack depth
- **Type checking**: Load/store/arithmetic operations verify operand types

### Array Safety

- **Negative size**: `newarray` rejects negative sizes
- **Bounds checking**: `iaload`, `iastore` check index bounds
- **Null checking**: Array operations verify valid references

### Memory Safety

- **Heap bounds**: All heap accesses validate handles
- **Field access**: Object field operations verify valid objects

### Instruction Safety

- **Invalid IP**: VM detects out-of-bounds instruction pointers
- **Unknown function**: Function calls verify target exists

---

## Compilation Process

The Inklusif compiler translates source code to bytecode in several phases:

```
+-------------------+
|   Source Code     |  (.ink file)
+-------------------+
         |
         v
+-------------------+
|      Parser       |  AST generation
+-------------------+
         |
         v
+-------------------+
|  Code Generator   |  Bytecode emission
+-------------------+
         |
         v
+-------------------+
|    Bytecode       |
+-------------------+
```

### Key Transformations

1. **Variables**: Local variables are assigned indices (0, 1, 2, ...)
2. **Expressions**: Converted to stack-based operations (postfix)
3. **Control flow**: Converted to conditional/unconditional jumps with absolute targets
4. **Functions**: Each function becomes a named code block
5. **Constants**: Large literals stored in constant pool, referenced by index

---

## Examples

### Hello World

```ink
fun main() : int {
    write("Hello, World!", 1);
    => 0;
}
```

```
header {
}

fun main {
    sconst "Hello, World!"  ; push string
    invoke_write 1          ; Print it
    iconst 0                ; Return value
    ireturn
}
```
