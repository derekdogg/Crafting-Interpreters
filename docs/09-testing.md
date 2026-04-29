# Testing Infrastructure

The project uses an in-process test runner that executes automatically on application startup. There is no external test framework — all assertions and comparisons are hand-coded in Pascal.

## Test Architecture

```
FormCreate
    │
    ├── RunSampleFiles()        // 40 custom .lox samples
    │   ├── samples/*.lox       // expect INTERPRET_OK
    │   └── samples/errors/*.lox // expect runtime errors
    │
    ├── RunOfficialTests()      // 148 tests from test/
    │   └── test/**/*.lox       // directive-driven expectations
    │
    └── (Test.pas)              // Unit tests called manually
```

## Sample File Tests (`RunSampleFiles`)

Located in `samples/`. Each file is executed via `InterpretResult()` and must produce `INTERPRET_OK` (no error). Error samples in `samples/errors/` must produce `INTERPRET_RUNTIME_ERROR`.

Categories:
- **Basic**: hello, arithmetic, variables, strings, scoping, control_flow, functions, closures
- **Advanced**: arrays, arrays_advanced, records, native_objects, modulo
- **GC**: 13 files exercising collection under various conditions (stress, interning, upvalues, args, edge cases, gray stack, arrays, records, torture)
- **Errors**: 11 files testing expected runtime errors (type errors, arity, undefined vars, stack overflow, array bounds)

## Official Test Suite (`RunOfficialTests`)

Located in `test/` with 20 subdirectories. Tests use comment directives to declare expectations:

### Directives

| Directive | Meaning |
|-----------|---------|
| `// expect: <value>` | Expected print output line |
| `// expect runtime error: <msg>` | Expected runtime error message |
| `// [line N] Error` | Expected compile error at line N |
| `// [c line N] Error` | Compile error (alternative form) |
| `// Error at ...` | Compile error at current line |

### Test Runner Logic

```
1. Read all lines of the .lox file
2. Parse directives from comments
3. Execute via InterpretResult()
4. Compare:
   - Result code matches expected (OK/compile error/runtime error)
   - PrintOutput lines match all "expect:" directives in order
   - ErrorStr contains expected error messages
5. Report pass/fail per file
```

Files with no directives are skipped. This allows the test directory to contain files that are parsed but not yet runnable.

### Categories (148 tests)

| Directory | Tests | Coverage |
|-----------|-------|----------|
| `assignment/` | 8 | Associativity, globals/locals, invalid targets |
| `block/` | 2 | Empty blocks, scoping |
| `bool/` | 2 | Equality, not |
| `call/` | 4 | Calling non-callable types |
| `closure/` | 11 | Capture, shadowing, nesting, reuse |
| `comments/` | 4 | Line comments, EOF, unicode |
| `for/` | 11 | Syntax, scoping, closures, return |
| `function/` | 12 | Parameters, recursion, limits, errors |
| `if/` | 10 | If/else, dangling else, truth |
| `limit/` | 4 | Stack overflow, loop size, locals/upvalues |
| `logical_operator/` | 4 | And/or evaluation |
| `nil/` | 1 | Literal nil |
| `number/` | 3 | Literals, NaN equality |
| `operator/` | 20 | All operators, type errors |
| `print/` | 1 | Missing argument |
| `regression/` | 1 | Bug fixes |
| `return/` | 6 | Return semantics |
| `string/` | 4 | Literals, multiline, errors |
| `variable/` | 18 | Scoping, shadowing, undefined |
| `while/` | 7 | Syntax, closures, return |

## Unit Tests (`Test.pas`)

Called manually (not auto-run). Tests are organized by subsystem:

### Memory & Allocation
- `TestAddValueConstant` — grow value array to MAX_SIZE
- `TestAllocateArray` — verify array growth behavior
- `TestAddConstant_upToMax` / `TestAddConstantLong` — OP_CONSTANT vs OP_CONSTANT_LONG threshold

### Strings & Values
- `TestStringEqual` / `TestStringUnequal` — content comparison
- `TestValuesEqual` — cross-type equality
- `TestStringInterning` — identical content → same pointer

### Stack
- `TestStackPush` / `TestStackPop` / `TestStackPeek` — 1000 element operations

### Hash Table
- `TestTable` — set, get, delete, tombstone handling, FindString
- `TestTableResize` — 50 keys force resize, verify all survive

### Interpreter Integration
- `TestInterpreter` — arithmetic, unary, booleans, comparisons, strings, runtime errors
- `TestGlobals` — var declaration, assignment, multiple globals
- `TestLocals` — blocks, shadowing, nested scopes, duplicate errors
- `TestControlFlow` — if/else, and/or, while, for, nested loops
- `TestFunctions` — declaration, return, arity, first-class, recursion, clock()
- `TestClosures` — capture, shadowing, counter pattern, shared upvalues

### Garbage Collection (5 parts)
1. **Smoke tests** — basic allocation/free cycles
2. **Stress GC** — heavy allocation under DEBUG_STRESS_GC
3. **Direct VM checks** — verify `BytesAllocated` via native functions
4. **Script-level** — `assert()` + `collectGarbage()` in Lox code
5. **Memory reclamation** — verify `objectsAllocated()` decreases after GC

### Assertion Helpers

```pascal
procedure AssertNumber(result: TInterpretResult; expected: Double);
procedure AssertBoolean(result: TInterpretResult; expected: Boolean);
procedure AssertNil(result: TInterpretResult);
procedure AssertStringOK(result: TInterpretResult; expected: String);
procedure AssertOutput(result: TInterpretResult; expected: String);
procedure AssertRuntimeError(result: TInterpretResult; expectedMsg: String);
procedure AssertCompileError(result: TInterpretResult);
```

## Native Object Tests (`NativeObjectTestUnit.pas`)

Accessed via Button3 in the GUI. Tests the native object registry pattern independently:

1. Initialize registry
2. Register `StringList` and `IntList` classes with methods
3. Create instances, call methods, verify results
4. Free registry

This validates the FFI mechanism without needing the full VM.

## Running Tests

Tests auto-execute on `FormCreate`:
1. All sample files are run → results in Memo2
2. All official tests are run → pass/fail/skip counts
3. Any failures are listed with expected vs actual output

To run manually: simply launch the application. Results appear immediately in the output memo.

## Build & Compile

- **IDE**: Delphi (Win32 target)
- **Build task**:  NA
- **Debug flags**: `{$ASSERTIONS ON}`, `{$DEFINE DEBUG_LOG_GC}`, `{$DEFINE DEBUG_STRESS_GC}`
- **Output**: `Win32/Debug/` directory
