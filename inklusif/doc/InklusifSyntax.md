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

# Lexical Elements

**identifier:**

![identifier](/assets/diagram/identifier.png)

```
identifier
         ::= letter ( letter | digit | '_' )*
```

**integer:**

![integer](assets/diagram/integer.png)

```
integer  ::= digit+
```

**float:**

![float](assets/diagram/float.png)

```
float    ::= digit+ '.' digit+
```

**string:**

![string](assets/diagram//string.png)

```
string   ::= '"' any_char_except_quote '"'
```

**char:**

![char](assets/diagram/char.png)

```
char     ::= "'" any_char "'"
```