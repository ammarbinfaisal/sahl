# Sahl Documentation

Welcome to the documentation for the Sahl programming language. Below, you'll find detailed information about the language features and syntax.

## Table of Contents

<table>
  <tr>
    <td width=33% valign=top>
      <table> <tr> <td> <b><a>Basics</a></b> </td> </tr> </table>
      <ul>
        <li><a href="#hello-world">Hello World</a></li>
        <li><a href="#functions">Functions</a></li>
        <li><a href="#variables">Variables</a></li>
        <li><a href="#types">Types</a></li>
        <li><a href="#operators">Operators</a></li>
      </ul>
    </td>
    <td width=33% valign=top>
        <table> <tr> <td> <b><a href="#statements">Statements</a></b> </td> </tr> </table>
        <ul>
          <li><a href="#if">if</a></li>
          <li><a href="#while">while</a></li>
          <li><a href="#for">for</a></li>
          <li><a href="#match">match</a></li>
          <li><a href="#destructuring">destructuring</a></li>
        </ul>
      </ul>
    </td>
    <td width=33% valign=top>
      <table> <tr> <td> <b><a href="#expressions">Expressions</a></b> </td> </tr> </table>
      <ul>
        <li><a href="#function-calls">Function Calls</a></li>
        <li><a href="#array-access">Array Access</a></li>
        <li><a href="#make">Make</a></li>
        <li><a href="#chan-recvsend">Chan Recv/Send</a></li>
      </ul>
    </td>
    <td width=33% valign=top>
      <table> <tr> <td> <b><a>More</a></b> </td> </tr> </table>
      <ul>
        <li><a href="#channels">Channels</a></li>
        <li><a href="#extern">Extern</a></li>
        <li><a href="#examples">Examples</a></li>
      </ul>
    </td>
  </tr>
</table>

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
let myTuple = (10, "Hello",); // trailing comma is nessesary
```

### Map

```kt
let myMap = map<int, string>;
```

### Chan

```kt
let myChannel = chan<int>;
```

### Type Constructors

```rs
type Person = PersonS(string) | PersonI(int);
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

### `match`

Works for types with constructors.

```rs
type Person = PersonS(string) | PersonI(int);

fun main() {
    let p1 = PersonS("John");
    let p2 = PersonI(20);
    let arr_p = [p1, p2];
    for p in arr_p {
        match p {
            PersonS(name) -> {
                print(name, "\n");
            }
            PersonI(age) -> {
                print(age, "\n");
            }
        }
    }
}
```

### destructuring

```rs
let tuple = (10, 20,);
let (a, b,) = tuple;
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

| Operator             | Description                             |
| -------------------- | --------------------------------------- |
| `()`                 | Parentheses                             |
| `[]`                 | Array access                            |
| `!`, `~`, `-`        | Logical NOT, bitwise NOT, unary minus   |
| `*`, `/`, `%`        | Multiplication, division, remainder     |
| `+`, `-`             | Addition, subtraction                   |
| `<<`, `>>`           | Bitwise shift left, bitwise shift right |
| `&`                  | Bitwise AND                             |
| `^`                  | Bitwise XOR                             |
| `\|`                 | Bitwise OR                              |
| `&&`                 | Logical AND                             |
| `\|\|`               | Logical OR                              |
| `==`, `!=`           | Equality, inequality                    |
| `<`, `<=`, `>`, `>=` | Comparison                              |
| `=`                  | Assignment                              |

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

## Extern

The `extern` keyword is used for importing external c functions. Example:

```kt
// prog.sahl

extern fun exp(a: double) -> double {}

fun main() {
    let a = 10.0;
    let b = exp(a);
    print(b, "\n"); // 22026.465795
}
```

compile with:

```bash
./frontend/target/release/sahl file.sahl -n 2>exe.ll
clang -lm -O3 rt.c exe.ll math.c -o exe
```

```bash
./exe
```

The `sahl` keyword runs a function in a new coroutine.

## Examples

Explore more examples in the [samples](samples/) folder. Happy coding in Sahl!
