# Architecture Overview

This project is a bytecode virtual machine implementation of the Lox language from Robert Nystrom's *Crafting Interpreters*, ported from C to Delphi Pascal. It lives almost entirely in a single unit — `Chunk_Types.pas` — which contains the scanner, compiler, VM, garbage collector, hash table, and all supporting data structures.

## Divergences from the C Implementation

| Aspect | clox (C) | This project (Delphi) |
|--------|----------|----------------------|
| Allocation | `reallocate()` wrapping `realloc` | `Allocate()` wrapping `ReallocMem` with zeroing and GC trigger |
| Object header | `struct Obj` with `#define` casts | `TObj` record; pointer-math casts (`pObjString(obj)`) |
| Tagged value | `NaN-boxing` or struct union | Variant record `TValue` with `case TValueKind of` |
| String storage | `ObjString` + flexible array (C99) | `TObjString` + `TAnsiCharArray = Array[0..0] of AnsiChar` |
| Stack | Fixed `Value stack[STACK_MAX]` | Dynamically grown via raw `ReallocMem` (never triggers GC) |
| Constant pool overflow | N/A (limited to 256) | 24-bit `_LONG` opcodes (`OP_CONSTANT_LONG`, `OP_DEFINE_GLOBAL_LONG`, etc.) |
| Error reporting | `fprintf(stderr, ...)` | `RuntimeErrorStr` / `Parser.ErrorStr` strings returned in `TInterpretResult` |
| GC logging | `#ifdef DEBUG_LOG_GC` to stderr | `{$IFDEF DEBUG_LOG_GC}` writing to `gc.log` file |
| Test runner | External scripts | In-process: `RunSampleFiles()` + `RunOfficialTests()` auto-run on `FormCreate` |
| Extensions | None | Arrays, record types, native object wrapping |

## Module Layout (all in `Chunk_Types.pas`)

```
┌─────────────────────────────────────────────────┐
│  Interface                                       │
│  ├── Constants (opcodes, limits, chars)          │
│  ├── Type declarations (enums, records, ptrs)    │
│  ├── Assertion procedure signatures              │
│  └── Public function/procedure signatures        │
├─────────────────────────────────────────────────┤
│  Implementation                                  │
│  ├── Assertions (~200 lines)                     │
│  ├── Memory: Allocate, AllocateArray, ClearMem   │
│  ├── Objects: CreateString, newFunction, etc.    │
│  ├── Value helpers: ValueToStr, ValuesEqual      │
│  ├── Chunk: init/free/write/emit                 │
│  ├── Stack: Init/Free/Push/Pop/Peek              │
│  ├── VM: InitVM, InterpretResult, Run, FreeVM    │
│  ├── GC: Mark/Trace/Sweep/Collect                │
│  ├── Native functions: clock, arrays, assert     │
│  ├── Scanner: InitScanner, ScanToken             │
│  ├── Compiler: compile, declaration, parsPrec    │
│  └── Hash table: Init/Set/Get/Delete/FindString  │
└─────────────────────────────────────────────────┘
```

## Execution Pipeline

```
Source (PAnsiChar)
       │
       ▼
┌─────────────┐
│   Scanner    │  → Token stream
└──────┬──────┘
       ▼
┌─────────────┐
│  Compiler    │  → pObjClosure (bytecode + constants)
│  (Pratt)     │
└──────┬──────┘
       ▼
┌─────────────┐
│     VM       │  → TInterpretResult (ok/error + output)
│  (dispatch)  │
└─────────────┘
```

The entry point is `InterpretResult(source)`:
1. Initializes VM (`InitVM`)
2. Compiles source → `pObjClosure` (or nil on error)
3. Pushes closure onto the stack, sets up call frame 0
4. Calls `Run` (the dispatch loop)
5. Returns result with output/error strings
6. Frees VM (`FreeVM`) in a `finally` block

## Global State

The interpreter uses unit-level globals rather than passing context everywhere:

- `VM : pVirtualMachine` — the virtual machine instance
- `Scanner : TScanner` — scanner state (start/current pointers, line)
- `Parser : TParser` — current/previous tokens, error state
- `Current : pCompiler` — the active compiler (linked list for nesting)

These are initialized in `InitVM` / `compile` and cleaned up in `FreeVM`.

## GUI Shell (`Main.pas`)

The VCL form provides:
- **Memo1**: Source editor
- **Memo2**: Output panel
- **Button1** ("Run"): Executes Memo1 contents via `InterpretResult`
- **Button3**: Runs native object tests
- **FormCreate**: Auto-runs all sample files + 148 official tests, displaying pass/fail in Memo2

## Assertion Hardening

Unlike the original C implementation which uses minimal assertions, this port has extensive runtime checks at function entry, mid-point, and exit. Every allocation, stack operation, and pointer dereference is guarded. These are compiled in via `{$ASSERTIONS ON}` and serve as both documentation and safety nets during development.
