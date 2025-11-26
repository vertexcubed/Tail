# Tail

## Info
Tail (short for Tail is Another Imperative Language) is a general purpose imperative scripting language implemented in Rust. 
It features static typing with type inference, explicit reference types, and row polymorphism to allow you to write clean but safe code. 
Tail is compiled down to bytecode IR, allowing it to run fast and efficiently.

## Features 
- Simple, clean syntax that’s easy to read and write
- HM Type Inference
- First class functions/closures
- Row polymorphic structs—write functions on structs with explicit fields
- Pass by value semantics
- Explicit heap-allocated references
- A simple stack based VM to execute bytecode

### Current Status

- [x] Bytecode VM
  - [x] Primitive types - ints, bools, etc.
  - [x] Functions
    - [ ] Closures
  - [x] References
  - [x] Structs
- [ ] Type Checking
  - [x] Type semantics
  - [ ] Basic type checking
  - [ ] Type inference
