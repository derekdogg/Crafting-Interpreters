# Crafting Interpreters — Bytecode VM in Delphi Pascal

A bytecode interpreter for the Lox language, following Bob Nystrom's [Crafting Interpreters](https://craftinginterpreters.com/) (Chapters 14–26), implemented in Delphi Pascal.

## Features

- **Scanner & Compiler** — Single-pass Pratt parser compiling to bytecode
- **Virtual Machine** — Stack-based VM with call frames, closures, and upvalues
- **Garbage Collection** — Mark-sweep GC with tricolor marking and gray stack worklist
- **String Interning** — Hash table with weak references for automatic deduplication
- **Native Functions** — `clock()`, `collectGarbage()`, `assert()`, `bytesAllocated()`, `objectsAllocated()`

## Chapters Implemented

| Chapter | Topic |
|---------|-------|
| 14–15 | Chunks, constants, bytecode encoding (incl. OP_CONSTANT_LONG) |
| 16 | Scanner |
| 17 | Pratt parser and compiler |
| 18 | Value types (numbers, booleans, nil) |
| 19 | Strings with flexible array member allocation |
| 20 | Hash tables with linear probing and tombstones |
| 21 | Global variables |
| 22 | Local variables and block scoping |
| 23 | Control flow (if/else, and/or, while, for) |
| 24 | Functions, call frames, native functions |
| 25 | Closures and upvalues |
| 26 | Garbage collection (mark-sweep) |

## Project Structure

```
Chunk_Types.pas      — Core unit: scanner, parser, compiler, VM, GC, hash table
Main.pas / Main.dfm  — GUI form with interpreter REPL and auto-run test suite
InterpreterGui.dpr   — Delphi project file
samples/             — Lox test programs (auto-run on startup)
samples/errors/      — Expected-error test programs (auto-run on startup)
```

## Testing

All sample files run automatically when the application starts:

- **15 samples** in `samples/` — expected to pass (`INTERPRET_OK`)
- **8 error tests** in `samples/errors/` — expected to produce runtime errors

Results are displayed in Memo2. You can also type Lox code into Memo1 and click **Run**.

### Sample Programs

| File | Coverage |
|------|----------|
| hello.lox | Basic print |
| arithmetic.lox | Operators, precedence, comparisons, logical and/or |
| variables.lox | Declaration, assignment, type reassignment, nil default |
| strings.lox | Concatenation, interning, loop building |
| scoping.lox | Blocks, shadowing, nested scopes |
| control_flow.lox | if/else, while, for, nested loops |
| functions.lox | Declarations, return, recursion, first-class, implicit nil, early return |
| closures.lox | Capture, counters, shared upvalues, nested closures |
| counter.lox | Closure-based counter pattern |
| fibonacci.lox | Recursive and iterative fibonacci |
| gc_basic.lox | Live data survives, dead temporaries reclaimed |
| gc_functions.lox | Functions and closures survive GC |
| gc_reclaim.lox | Memory reclamation verified via `bytesAllocated()` |
| gc_stress.lox | Heavy allocation patterns under GC pressure |

### Error Tests

| File | Expected Error |
|------|---------------|
| division_by_zero.lox | Division by zero |
| type_error_add.lox | String + number type mismatch |
| negate_string.lox | Negate non-number |
| multiply_booleans.lox | Multiply non-numbers |
| wrong_arity_few.lox | Too few arguments |
| wrong_arity_many.lox | Too many arguments |
| call_non_function.lox | Calling a non-callable value |
| undefined_variable.lox | Undefined variable access |
| stack_overflow.lox | Infinite recursion |

## GC Hardening

The garbage collector has been hardened with:

- **DEBUG_STRESS_GC** — Triggers `CollectGarbage` on every growth allocation
- **DEBUG_LOG_GC** — Logs allocate/free/mark/blacken events to `gc.log` with summary stats
- **Push/pop protection** at all 7 GC-sensitive allocation sites
- **NextGC floor** of 1024 bytes to prevent zero-threshold assertions

## Build

Built with Delphi (Win32 target). The project builds via the IDE or with the configured build task.
