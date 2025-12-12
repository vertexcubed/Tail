# Tail

## Info
Tail (short for Tail is Another Imperative Language) is a general purpose imperative scripting language implemented in Rust. 
It features static typing with type inference, explicit reference types, and a simple but clean syntax, allowing you to write clean but safe code. 
Tail is compiled down to bytecode IR, allowing it to run fast and efficiently.

## Features 
- Straightforward syntax that’s easy to read and write
- HM Type Inference
- First class functions/closures
- Pass by value semantics
- Explicit heap-allocated references
- A simple stack based VM to execute bytecode

## Example

```rs
let factorial(n) = {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}
let add_to(value) = {
    *value += 5
}

let foo = ref 2
add_to(foo)
*foo = *foo + factorial(10)
```
