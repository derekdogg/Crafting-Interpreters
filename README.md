# Crafting Interpreters — Bytecode VM in Delphi Pascal

A bytecode interpreter for the Lox language, following Bob Nystrom's [Crafting Interpreters](https://craftinginterpreters.com/) (Chapters 14–26), implemented in Delphi Pascal.

## Features

- **Scanner & Compiler** — Single-pass Pratt parser compiling to bytecode
- **Virtual Machine** — Stack-based VM with call frames, closures, and upvalues
- **Garbage Collection** — Mark-sweep GC with tricolor marking and gray stack worklist
- **String Interning** — Hash table with weak references for automatic deduplication
- **Arrays** — Dynamic arrays via native functions (`newArray`, `arrayPush`, `arrayPop`, `arrayGet`, `arraySet`, `arrayLen`, `arrayRemove`)
- **Long-Operand Opcodes** — 24-bit constant indices for globals, closures, and literals (16M constant limit)
- **Native Functions** — `clock()`, `collectGarbage()`, `assert()`, `bytesAllocated()`, `objectsAllocated()`, plus 7 array functions

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
| — | Arrays (native function API, GC-integrated) |
| — | Modulo operator (`%`) |
| — | Long-operand opcodes (`OP_*_LONG`) for >255 constants per chunk |

## Project Structure

```
Chunk_Types.pas      — Core unit: scanner, parser, compiler, VM, GC, hash table
Main.pas / Main.dfm  — GUI form with interpreter REPL and auto-run test suite
InterpreterGui.dpr   — Delphi project file
samples/             — Custom Lox test programs (auto-run on startup)
samples/errors/      — Expected-error test programs (auto-run on startup)
test/                — Official Crafting Interpreters test suite (auto-run on startup)
```

## Testing

All tests run automatically when the application starts. Results are displayed in Memo2. You can also type Lox code into Memo1 and click **Run**.

### Official Test Suite

148 tests from the [official Crafting Interpreters test suite](https://github.com/munificent/craftinginterpreters/tree/master/test), matching the **chap26_garbage** level (all features except classes/inheritance). Tests are organized across 20 categories:

| Category | Tests | Coverage |
|----------|-------|----------|
| assignment | 8 | Associativity, globals, locals, grouping, invalid targets |
| block | 2 | Empty blocks, scoping |
| bool | 2 | Equality, logical not |
| call | 4 | Calling non-callable types (bool, nil, num, string) |
| closure | 11 | Capture, shadowing, nesting, reuse, unused closures |
| comments | 4 | Line comments, EOF, unicode |
| for | 11 | Syntax, scoping, closures, return, error recovery |
| function | 12 | Parameters, recursion, mutual recursion, limits, errors |
| if | 10 | If/else, dangling else, truth, var/fun in branches |
| limit | 4 | Stack overflow, loop too large, too many locals/upvalues |
| logical_operator | 4 | And/or evaluation, truthiness |
| nil | 1 | Literal nil |
| number | 3 | Literals, leading dot, NaN equality |
| operator | 20 | Arithmetic, comparison, equality, type errors |
| print | 1 | Missing argument error |
| regression | 1 | Bug regression (#40) |
| return | 6 | Return from functions, after control flow, at top level |
| string | 4 | Literals, multiline, unterminated, error after multiline |
| variable | 18 | Scoping, shadowing, undefined, duplicate, initializer |
| while | 7 | Syntax, closures, return, var/fun in body |
| *(top-level)* | 3 | Precedence, empty file, unexpected character |

The test runner parses `// expect:`, `// expect runtime error:`, and `// [line N] Error` comments from each `.lox` file and verifies actual output matches expected results.

### Custom Sample Tests

**25 samples** in `samples/` — expected to pass (`INTERPRET_OK`):

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
| modulo.lox | Modulo operator with integers, floats, negative numbers |
| arrays.lox | Array creation, push, pop, get, set, len, remove |
| arrays_advanced.lox | Nested arrays, arrays in closures, arrays as function args |
| gc_basic.lox | Live data survives, dead temporaries reclaimed |
| gc_functions.lox | Functions and closures survive GC |
| gc_reclaim.lox | Memory reclamation verified via `objectsAllocated()` |
| gc_stress.lox | Heavy allocation patterns under GC pressure |
| gc_interning.lox | String interning consistency, intern table under churn |
| gc_upvalue_closing.lox | Closed upvalues, nested closing, capture-by-reference, factories |
| gc_args_and_temps.lox | GC between args, mid-expression temps, recursive arg building |
| gc_scopes_and_natives.lox | Native `clock()` survives, deep scopes, gray stack, loop capture |
| gc_edge_cases.lox | Open upvalue list, closing order, rapid create/discard, name reuse, large constant pools |
| gc_torture.lox | Concat chains, nested defs, slot reuse, fib, ping-pong, closure spam, alternating alloc/collect |
| gc_gray_stack.lox | Gray stack growth under heavy marking pressure |
| gc_arrays.lox | Array elements survive GC, nested array marking, array in closures |

**11 error tests** in `samples/errors/` — expected to produce errors:

| File | Expected Error |
|------|---------------|
| type_error_add.lox | String + number type mismatch |
| negate_string.lox | Negate non-number |
| multiply_booleans.lox | Multiply non-numbers |
| wrong_arity_few.lox | Too few arguments |
| wrong_arity_many.lox | Too many arguments |
| call_non_function.lox | Calling a non-callable value |
| undefined_variable.lox | Undefined variable access |
| stack_overflow.lox | Infinite recursion |
| array_out_of_bounds.lox | Array index out of bounds |
| array_pop_empty.lox | Pop from empty array |
| array_not_array.lox | Array operation on non-array value |

## GC Hardening

The garbage collector has been hardened with:

- **DEBUG_STRESS_GC** — Triggers `CollectGarbage` on every growth allocation
- **DEBUG_LOG_GC** — Logs allocate/free/mark/blacken events to `gc.log` with summary stats
- **Push/pop protection** at all 7 GC-sensitive allocation sites
- **NextGC floor** of 1024 bytes to prevent zero-threshold assertions
- **10 dedicated GC test files** (6 new) covering interning, upvalue closing, argument temporaries, nested scopes, native functions, open upvalue linked lists, slot reuse, mutual recursion, alternating alloc/collect torture scenarios, and array element marking

## Build

Built with Delphi (Win32 target). The project builds via the IDE or with the configured build task.
