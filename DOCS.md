# Sahl Documentation

Welcome to the documentation for the Sahl programming language. Below, you'll find detailed information about the language features and syntax.

## Table of Contents

- [Hello World](#hello-world)
- [Functions](#functions)
- [Variables](#variables)
- [Types](#types)
- [Statements](#statements)
- [Expressions](#expressions)
- [Channels](#channels)
- [Examples](#examples)

## Hello World

Let's start with the traditional "Hello World" program:

```kt
fun main() {
    print("Hello World! \n");
}
```

This basic program defines a `main` function and prints the classic greeting.

## Functions

Functions are fundamental in Sahl. Here's an example of a function adding two integers:

```kt
fun add(a: int, b: int) -> int {
    return a + b;
}
```

If the return type is unspecified, it defaults to `void`.

## Variables

Variables store and manipulate data. Check this example:

```kt
fun main() {
    let a = 10;
    let b = 20;
    let c = a + b;
    print(c, "\n");
}
```

Variable types are inferred from assigned values.

## Types

Sahl supports various types, including basic ones like `int`, `double`, `bool`, `string`, and `char`. It also includes more complex types like arrays, tuples, maps, and channels.

### Arrays

```kt
let a = [1, 2, 3];
```

### Tuple

```kt
let myTuple = (10, "Hello");
```

### Map

```kt
let myMap = map<int, string>;
```

### Chan

```kt
let myChannel = chan<int>;
```

### Type Declaration

```kt
type Tree = (int, [Tree]);

fun makeTree() -> Tree {
    return (10, [(20, []), (30, [])]);
}
```

## Statements

Essential statements include `if`, `while`, `for`, `break`, `continue`, and `return`. Let's explore a few:

### `if`

```kt
if a > 0 {
    a = a + 1;
}
```

### `while`

```kt
while a > 0 {
    a = a - 1;
}
```

### `for`

```kt
for i in 0..10 {
    print(i, "\n");
}
```

### `break` and `continue`

Used inside loops for exiting or continuing to the next iteration.

### `return`

Used inside functions to return a value.

## Expressions

Expressions involve operators, function calls, array access, and more.

### Operators

Math operators: `+`, `-`, `*`, `/`, `%`. Comparison operators: `==`, `!=`, `>`, `<`, `>=`, `<=`. Logical and bitwise operators are also supported.
The precedence of operators is listed below, from highest to lowest:

| Operator | Description |
| --- | --- |
| `()` | Parentheses |
| `[]` | Array access |
| `!`, `~`, `-` | Logical NOT, bitwise NOT, unary minus |
| `*`, `/`, `%` | Multiplication, division, remainder |
| `+`, `-` | Addition, subtraction |
| `<<`, `>>` | Bitwise shift left, bitwise shift right |
| `&` | Bitwise AND |
| `^` | Bitwise XOR |
| `\|` | Bitwise OR |
| `&&` | Logical AND |
| `\|\|` | Logical OR |
| `==`, `!=` | Equality, inequality |
| `<`, `<=`, `>`, `>=` | Comparison |
| `=` | Assignment |


### Function Calls

```kt
let result = add(10, 20);
```

### Array Access

```kt
let value = a[0];
```

### Make

```kt
let newArray = make([int], 10);
```

`make` is used for creating arrays, maps, and channels.

### Chan Recv/Send

```kt
let c = make(chan<int>);
c <- 10;
let value = <-c;
```

Check [Channels](#channels).

## Channels

Channels facilitate communication between coroutines. Example:

```kt
fun sendVals(c: chan<int>, count: int, id: int) {
    let i = 0;
    while i < count {
        c <- i;
        i = i + 1;
    }
}

fun recvVals(c: chan<int>, count: int, id: int) {
    let i = 0;
    while i < count {
        print(<-c, " - ", id, "\n");
        i = i + 1;
    }
}

fun main() {
    let i = 0;
    while i < 100 {
        let a = make(chan<int>, 1000);
        sahl sendVals(a, 10000, i);
        sahl recvVals(a, 10000, i);
        i = i + 1;
    }
}
```

The `sahl` keyword runs a function in a new coroutine.

## Examples

Explore more examples in the [samples](samples/) folder. Happy coding in Sahl!