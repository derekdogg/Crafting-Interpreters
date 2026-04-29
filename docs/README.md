# Lox Bytecode VM — Delphi Implementation

Documentation for the Delphi Pascal implementation of the Lox bytecode virtual machine from *Crafting Interpreters* by Robert Nystrom.

## Table of Contents

1. [Architecture Overview](01-architecture-overview.md) — Project structure, execution pipeline, divergences from clox
2. [Data Types & Memory Layout](02-data-types-and-memory.md) — TValue, object system, allocation strategy
3. [Scanner](03-scanner.md) — Tokenization, keyword recognition, pointer-based lexing
4. [Compiler](04-compiler.md) — Pratt parsing, bytecode emission, variable resolution, closures
5. [Virtual Machine](05-virtual-machine.md) — Dispatch loop, call frames, instruction set
6. [Garbage Collector](06-garbage-collector.md) — Mark-sweep, tricolor marking, GC safety patterns
7. [Hash Tables & String Interning](07-hash-tables-and-interning.md) — FNV-1a, linear probing, intern set
8. [Extensions](08-extensions.md) — Arrays, records, native objects, long operands, modulo
9. [Testing](09-testing.md) — Test runner, sample files, official suite, unit tests
10. [Opcode Reference](10-opcode-reference.md) — Complete instruction set with encodings and stack effects
11. [Native Functions](11-native-functions.md) — TNativeFn, registration, dispatch, arrays, native objects

## Quick Reference

| Source File | Role |
|-------------|------|
| `Chunk_Types.pas` | Everything: scanner, compiler, VM, GC, hash table (~6200 lines) |
| `Main.pas` | VCL GUI shell, test runner |
| `Test.pas` | Unit tests (manual) |
| `NativeObjectTestUnit.pas` | Native object FFI tests |
| `InterpreterGui.dpr` | Application entry point |
| `samples/` | 40 Lox programs (positive + error cases) |
| `test/` | 148 official test suite files |
