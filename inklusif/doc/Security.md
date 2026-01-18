# Inklusif Security Model

This section describes the security principles of the language, the design choices that reduce common vulnerabilities, and the limitations of the current implementation.
Security in this context does not refer to cybersecurity, but to **language-level safety**, **error prevention**, and **reduction of undefined behavior**.

The goal of the language is to prevent as many programming errors as possible at **compile time**, rather than allowing them to appear at runtime. This reduces the risk of bugs, crashes, and vulnerabilities in programs written in the language.

---

## Language Design Choices

Programs that compile are guaranteed to respect a set of structural and understandable rules that prevent many classes of errors.

Key design choices include:

* Explicit syntax for all operations (assignments, calls, mutations) / __Check "InklusifSyntax.md" for more details__
* No implicit type conversions
* No implicit memory manipulation
* No hidden control flow mechanisms
* Clear separation between expressions and statements
* Explicit mutability (`let`) and immutability (`const`)

These choices reduce ambiguity in code interpretation and make program behavior more predictable, both for the compiler and for developers.

By avoiding implicit behaviors, the language minimizes the risk of unexpected side effects and unintended execution paths.

---

## Type Safety

The language implements a **strong type system**.

All variables, functions, and expressions have explicit and well-defined types.
Type checking is performed at compile time, preventing invalid programs from being executed.

Security benefits:

* Prevention of type confusion vulnerabilities
* Elimination of invalid data manipulation
* Compile-time detection of invalid operations
* Reduction of runtime crashes

Examples of prevented errors:

```ink
let x -> int = "hello";
let y -> bool = 42;
```

Functions and lambdas are also strictly typed, ensuring that:

* Arguments match parameter types
* Return values respect declared return types
* Invalid function calls are rejected at compile time

This approach is inspired by strongly typed languages such as **Haskell**, and **Rust**, where correctness and safety are enforced before execution.

---

## Memory Safety

The language does not expose low-level memory manipulation features.

There is:

* No manual memory allocation
* No direct memory access
* No manual memory freeing

All memory is handled by the virtual machine of the language.

Security benefits:

* Prevention of buffer overflows
* Prevention of use-after-free vulnerabilities
* Prevention of memory corruption
* Elimination of segmentation faults caused by invalid memory access

This design avoids entire classes of vulnerabilities commonly found in low-level languages such as **C** and **C++**.

---

## Control Flow Safety

The language enforces **structured and explicit control flow**.

Features include:

* No `goto` statements
* Explicit branching (`il`, `match`, `while`, `for`)

Pattern matching constructs require explicit case handling, reducing the risk of unhandled states.

Example:

```ink
match value {
    0 -> ...
    1 -> ...
    _ -> ...
}
```

This ensures:

* Predictable execution paths
* No undefined branching behavior
* Better reasoning about program logic

Explicit control flow improves both **security** and **maintainability**, making programs easier to audit and verify.

---

## Comparison With Other Languages We Inspired

### C / C++

These languages expose low-level memory access and manual memory management, leading to vulnerabilities such as:

* Buffer overflows
* Use-after-free
* Memory corruption
* Undefined behavior

Even though we wre inspired by C and C++, the language avoids these issues by **not exposing raw memory primitives**.

---

### Python / JavaScript

These languages use dynamic typing, which allows many errors to appear only at runtime, such as:

* Type confusion
* Unexpected `null` / `undefined` values
* Late detection of invalid operations

The language avoids these issues through **typing and compile-time checks**.

### Haskell

These language inspired the design of Inklusif through:

* Strong static typing
* Immutability by default
* Pattern matching
* Explicit side effects

The language adopts these principles while providing a more imperative and accessible syntax.

---

## Limitations and Future Improvements

The current implementation focuses on **compile-time safety**, but some security features are planned for future improvements:

* Dead code detection
* Unused variable detection

These improvements aim to further reduce vulnerabilities and improve program reliability.
