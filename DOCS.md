# Documention

## Table of Contents

- [Hello World](#hello-world)
- [Functions](#functions)
- [Variables](#variables)
- [Types](#types)
- [Statements](#statements)
- [Expressions](#expressions)
- [Channels](#channels)

## Hello World

```kt
fun main() {
    print("Hello World! \n")
}
```

## Functions

```kt
fun add(a: int, b: int) -> int {
    return a + b
}
```

If the return type is not specified it is assumed to be `void`.

## Variables

```kt
fun main() {
    let a = 10
    let b = 20
    let c = a + b
    print(c, "\n")
}
```

The type of the variable is inferred from the value assigned to it.

## Types

### Basic Types

- `int`
- `double`
- `bool`
- `string`
- `chat`

### Arrays

- `[T]` - array of type `T`
- for example `[int]` is an array of integers and `[[double]]` is an array of arrays of doubles

### Tuple

- `(T1, T2, ..., Tn)` - tuple of types `T1, T2, ..., Tn`
- for example `(int, double)` is a tuple of an integer and a double

### Map

- `map<K, V>` - map with keys of type `K` and values of type `V`
- for example `map<int, string>` is a map with keys of type `int` and values of type `string`

### Chan

- `chan<T>` - channel of type `T`
- check [channels](#channels)

## Statements

#### `if`

```kt
if a > 0 {
    a = a + 1
}
```

#### `while`

```kt
while a > 0 {
    a = a - 1
}
```

#### `for`

```kt
for i in 0..10 {
    print(i, "\n")
}
```

or the range can be have `=` like `0..=10` which means `0, 1, 2, ..., 10` otherwise it is `0, 1, 2, ..., 9`

#### `break` and `continue`

used inside `while` and `for` loops to break out of the loop or continue to the next iteration

#### `return`

used inside functions to return a value


## Expressions


### Operators

- the math operators are `+`, `-`, `*`, `/`, `%` 
- the comparison operators are `==`, `!=`, `>`, `<`, `>=`, `<=`
- the logical operators are `&&`, `||`, `!`
- the bitwise operators are `&`, `|`, `^`, `<<`, `>>`
- the assignment operators is only `=` for now

### Function Calls

```kt
fun add(a: int, b: int) -> int {
    return a + b
}

fun main() {
    let a = add(10, 20)
    print(a, "\n")
}
```

### Array Access

```kt
fun main() {
    let a = [1, 2, 3]
    print(a[0], "\n")
}
```

### Make

```kt
fun main() {
    let a = make([int], 10)
    print(a[0], "\n")
}
```

make is used to create arrays, tuples and maps and channels


### Chan Recv/Send

```kt
fun main() {
    let c = make(chan<int>)
    c <- 10
    let a = <-c
    print(a, "\n")
}
```

check [channels](#channels)


## Channels


```kt
fun sendvals(c: chan<int>, count: int, id: int) {
    let i = 0
    while i < count {
        c <- i
        i = i + 1
    }
}

fun recvvals(c: chan<int>, count: int, id: int) {
    let i = 0
    while i < count {
        print(<-c, " - ", id, "\n")
        i = i + 1
    }
}

fun main() {
    let i = 0
    while i < 100 {
        let a = make(chan<int>, 1000)
        sahl sendvals(a, 10000, i)
        sahl recvvals(a, 10000, i)
        i = i + 1
    }
}
```

The `sahl` keyword is used to run a function in a new coroutine.

### Examples

check [samples](samples/) folder

