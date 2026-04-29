# Extensions Beyond the Book

This implementation extends the original *Crafting Interpreters* clox with arrays, record types, native object wrapping, and additional native functions.

## Arrays

### Language Interface

Arrays are accessed through native functions rather than syntax:

```lox
var a = newArray();
arrayPush(a, "hello");
arrayPush(a, 42);
print arrayGet(a, 0);     // "hello"
print arrayLen(a);         // 2
arraySet(a, 1, 99);
var removed = arrayRemove(a, 0);
var last = arrayPop(a);
```

### Implementation

```pascal
TObjArray = record
  Obj:      TObj;
  Count:    integer;
  Capacity: integer;
  Elements: pValue;    // heap array managed via Allocate
end;
```

- **Growth**: Starts at 0 capacity, grows to 8, then doubles
- **GC tracing**: `BlackenObject` marks all `Elements[0..Count-1]`
- **Bounds checking**: `arrayGet`, `arraySet`, `arrayRemove` validate indices at runtime
- **`arrayRemove`**: Shifts elements left (O(n))

### Native Functions

| Function | Arity | Description |
|----------|-------|-------------|
| `newArray()` | 0 | Create empty array |
| `arrayPush(arr, val)` | 2 | Append, return array |
| `arrayPop(arr)` | 1 | Remove & return last |
| `arrayGet(arr, idx)` | 2 | Get by index |
| `arraySet(arr, idx, val)` | 3 | Set by index |
| `arrayLen(arr)` | 1 | Return count |
| `arrayRemove(arr, idx)` | 2 | Remove & return at index |

## Record Types

### Language Interface

```lox
record Point(x, y);

var p = Point(3, 4);
print p.x;           // 3
p.y = 10;
print p;             // Point(x: 3, y: 10)
```

Records are declared with `record Name(field1, field2, ...);`. The declaration creates a callable constructor. Field access and mutation use dot notation.

### Compilation

`recordDeclaration()` emits `OP_RECORD` with:
- 1 byte: field count
- N bytes: constant pool indices for each field name string
- 1 byte: constant pool index for the type name string

At runtime, `OP_RECORD` creates a `TObjRecordType` and pushes it onto the stack. It's then stored as a global variable (the constructor).

### Construction

When a record type is called (via `CallValue`), it:
1. Verifies argument count matches field count
2. Creates a `TObjRecord` instance
3. Copies argument values into the fields array
4. Returns the record instance

### Field Access

`OP_GET_PROPERTY` / `OP_SET_PROPERTY`:
1. Read the field name from the constant pool
2. Call `findFieldIndex` — linear scan of `recordType.fieldNames`
3. Access `fields[idx]` directly

This is O(n) in field count for lookup. For records with few fields (typical), this is effectively constant time.

### Printing

`ValueToStr` for records produces: `TypeName(field1: val1, field2: val2, ...)`

## Native Objects

### Purpose

Wraps arbitrary Delphi objects (like `TStringList`) so Lox code can interact with them via method calls.

### Architecture

```
┌─────────────────────────────┐
│  Lox Code                    │
│  var sl = StringList();      │
│  sl.add("hello");            │
│  print sl.count();           │
└──────────────┬──────────────┘
               │ OP_INVOKE
               ▼
┌─────────────────────────────┐
│  TNativeClassInfo            │
│  ├── name: "StringList"      │
│  ├── methods[]:              │
│  │   ├── "add" → SL_Add     │
│  │   ├── "get" → SL_Get     │
│  │   ├── "count" → SL_Count │
│  │   └── "remove" → SL_Remove│
│  └── destructor: SL_Destroy  │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│  TStringList (Delphi object) │
└─────────────────────────────┘
```

### Registration

```pascal
registerNativeClass('StringList', slMethods, SL_Destroy);
defineNative('StringList', stringListNative);
```

`registerNativeClass` creates a `TNativeClassInfo` in the global registry. A native function (`stringListNative`) serves as the constructor — it creates the Delphi object and wraps it in `TObjNativeObject`.

### Method Dispatch

`OP_INVOKE` performs:
1. Look up receiver (below args on stack)
2. Verify it's a native object
3. Find method by name in `classInfo.methods` (linear scan)
4. Call `method(instance, argCount, args)`
5. Replace args+receiver with result

### GC Behavior

`TObjNativeObject` is allocated and tracked by the GC, but the wrapped Delphi object (`instance: Pointer`) is **not traced**. When the GC sweeps a native object, it calls `destructor_` to free the Delphi side:

```pascal
okNativeObject: begin
  if Assigned(pObjNativeObject(obj)^.classInfo^.destructor_) then
    pObjNativeObject(obj)^.classInfo^.destructor_(pObjNativeObject(obj)^.instance);
  Allocate(Pointer(obj), SizeOf(TObjNativeObject), 0, vm.MemTracker);
end;
```

### Built-in: StringList

Available methods:

| Method | Args | Description |
|--------|------|-------------|
| `add(str)` | 1 string | Append to list |
| `get(idx)` | 1 number | Get item by index |
| `count()` | 0 | Return item count |
| `remove(idx)` | 1 number | Delete item at index |

## Additional Native Functions

Beyond the book's `clock()`:

| Function | Purpose |
|----------|---------|
| `collectGarbage()` | Force a GC cycle |
| `assert(cond [, msg])` | Runtime assertion |
| `bytesAllocated()` | Current tracked memory |
| `objectsAllocated()` | Count of live GC objects |

These are primarily for testing and debugging GC behavior from within Lox scripts.

## Long Operand Opcodes

The book limits constant pools to 256 entries. This implementation adds 24-bit variants for programs with many constants:

| Standard | Long | Operand Size |
|----------|------|-------------|
| `OP_CONSTANT` | `OP_CONSTANT_LONG` | 1 → 3 bytes |
| `OP_DEFINE_GLOBAL` | `OP_DEFINE_GLOBAL_LONG` | 1 → 3 bytes |
| `OP_GET_GLOBAL` | `OP_GET_GLOBAL_LONG` | 1 → 3 bytes |
| `OP_SET_GLOBAL` | `OP_SET_GLOBAL_LONG` | 1 → 3 bytes |
| `OP_CLOSURE` | `OP_CLOSURE_LONG` | 1 → 3 bytes |
| `OP_GET_PROPERTY` | `OP_GET_PROPERTY_LONG` | 1 → 3 bytes |
| `OP_SET_PROPERTY` | `OP_SET_PROPERTY_LONG` | 1 → 3 bytes |
| `OP_INVOKE` | `OP_INVOKE_LONG` | 1 → 3 bytes |

Encoding uses little-endian 24-bit integers via `IntToBytes`/`ByteToInt`:
```pascal
function IntToBytes(const value: Integer): TIntToByteResult;
begin
  Result.byte0 := Byte(value and $FF);
  Result.byte1 := Byte((value shr 8) and $FF);
  Result.byte2 := Byte((value shr 16) and $FF);
end;
```

## Modulo Operator

Added `%` operator (not in the book):
- Token: `TOKEN_PERCENT`
- Opcode: `OP_MODULO`
- Semantics: `A - Trunc(A / B) * B` (truncation toward zero, like C's `%` for integers)
- Works with floats: `7.5 % 2.5` → `0`
