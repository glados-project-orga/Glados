# Inklusif

**Inklusif** is a small functional-style programming language developed as an **Epitech project** by a group of 5 students !
The language provides pattern matching, loops, references, type traits, and basic data structures !

Every Inklusif file need an **".ink"** extension.

---

## Features

### Match

A pattern matching system similar to switch/case. Use `_` as the “other” case.

```jsx
fun showBinaryOrOther(x -> int) : void
{
    match x {
        0 => see("zero");
        1 => see("one");
        _ => see("other");
    }
}
```

---

### Loops

#### While Loop

```jsx
fun while_loop() : void {
    let i -> int = 0;

    while (i < 5) {
        i++;
    }
}
```

#### For Loop

```jsx
fun for_loop() : void {
    for (let i -> int = 0; i < 5; i++) {
        i++;
    }
}

fun for_loop_multiple_iterator() : void {
    let i -> int = 0;
    let j -> int = 0;
    let k -> int = 0;

    for (i < 5; i++, j++, k++) {
        i++;
    }
}
```

#### Loop Expression

```jsx
fun fact(nb -> int) : int
{
    res -> int = 0;
    fib -> int = loop
    {
        il (nb eq 1)
            => 1;
        il (nb sup 1) {
            res += nb * nb - 1;
            nb minus 1;
        }
        => res;
    }
}
```

#### Iterating Over Lists

```jsx
fun print_list_elems(list -> array[str]) : void {
    for elem in list :
        print elem;
}

fun main() -> void {
    list = ["hello", "world"];
    print_list_elems(list);
}

OUTPUT:
"hello"
"world"
```

---

### Try / Catch

Error handling with `throw` and `catch`.

```jsx
fun div(a -> int, b -> int) : int
{
    il (b eq 0)
        throw ValueError "division";
    elle
        => (a / b);
}

fun main() : void
{
    try {
        div 5 0;
    }
    catch value error as e {
        print e;
    }
}
```

---

### References / Heap

```jsx
fun modifyValue(&x -> int) : void {
    x = 20;
}

fun main() : void {
    int &a = 10;
    modifyValue(a);
    print a;
}

OUTPUT: 20
```

---

### Type Traits

```jsx
fun main() : void {
    int index = 0;
    string age = "age ";

    while (index < 10) {
        index.inc;
    }

    age.combine(index.to_string);
    print(age);
}

OUTPUT: "age 10"
```

---

### File Inclusion (Optional / Low Priority)

```jsx
#include "./hello_world.ink"

fun main() : void {
    hello_world();
}

OUTPUT: "hello_world"
```

---

### Structures, Enums, Typedefs

```jsx
struct item {
    weight -> int;
    name -> string;
};

enum item_list {
    SWORD = 0,
    BOW,
    WAND
};

typedef int Integer;
```

---

### Types

* Int
* Float (Bonus)
* String
* Array (iterable)
* Char
* Bool
* Lambda
* Tuple

**Variable Declaration Syntax**

```jsx
Let variableMutable -> int = 0
Const variableConst -> str = "string"
```

---

### Built-in Operators

* Arithmetic: `+ - * / mod`
* Comparison: `== != < > <= >=`
* Logical: `&& || !`
* Power: `^^`

---

### Summary

Inklusif provides:

* Pattern matching (`match`)
* Loops (`while`, `for`, `loop`)
* List iteration without indices
* Error handling (`throw` / `catch`)
* References and heap manipulation
* Type traits and built-in methods
* Basic file inclusion
* Structs, enums, typedefs
* Standard types and operators

This project was created collaboratively by a group of 5 Epitech students.