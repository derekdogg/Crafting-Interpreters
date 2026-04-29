# Data Types & Memory Layout

This document covers the type system, memory representation, and allocation strategy.

## Value Representation (`TValue`)

Lox is dynamically typed. Every runtime value is a `TValue` — a tagged union (Delphi variant record):

```pascal
TValue = record
  ValueKind: TValueKind;          // discriminator tag
  case TValueKind of
    vkNumber:  (NumberValue: Double);    // IEEE 754 double
    vkBoolean: (BooleanValue: Boolean);
    vkNull:    (NullValue: Byte);
    vkObject:  (ObjValue: pObj);         // pointer to heap object
end;
```

The `case` tag determines which union field is valid. Value constructors enforce this:

```pascal
function CreateNumber(Value: Double): TValue;
function CreateBoolean(Value: Boolean): TValue;
function CreateNilValue: TValue;
function CreateObject(value: pObj): TValue;
```

Unlike the book's NaN-boxing optimization, this uses a straightforward struct-with-tag. The trade-off is larger values (~16 bytes vs 8) but clearer code and no platform-specific bit tricks.

## Object System

All heap-allocated objects share a common header:

```pascal
TObj = record
  ObjectKind: TObjectKind;   // okString, okFunction, okClosure, ...
  IsMarked: Boolean;         // GC mark bit
  Next: pObj;                // intrusive linked list (all objects)
end;
```

Concrete object types embed `TObj` as their first field, enabling pointer casts:

```pascal
TObjString = record
  Obj: TObj;                 // ← same memory layout as TObj at offset 0
  length: integer;
  hash: uint32;
  chars: TAnsiCharArray;     // flexible array member
end;
```

This mirrors the C pattern of `struct Obj` as the first member. Casting between `pObj` and `pObjString` is valid because Delphi lays out record fields sequentially.

### Object Kinds

```pascal
TObjectKind = (
  okString,        // interned string with inline chars
  okFunction,      // compiled function (bytecode chunk)
  okNative,        // Delphi-implemented function
  okClosure,       // function + captured upvalues
  okUpvalue,       // single captured variable slot
  okArray,         // dynamic array of TValue
  okRecordType,    // schema: field names
  okRecord,        // instance: field values
  okNativeObject   // wrapped Delphi object (e.g., TStringList)
);
```

## Flexible Array Member Pattern

Strings use a Delphi idiom for variable-length inline data:

```pascal
TAnsiCharArray = Array[0..0] of AnsiChar;  // "flexible" via over-allocation
```

When allocating a string of length `N`:
```pascal
NewSize := SizeOf(TObjString) + Max(0, Len - 1);
AllocateString(Result, 0, NewSize, MemTracker);
Move(PAnsiChar(S)^, Result^.Chars[0], Len);
```

The `Array[0..0]` declaration satisfies the compiler; actual access beyond index 0 relies on `{$POINTERMATH ON}` and the over-allocated memory. No null terminator is stored — length is tracked explicitly.

## Function Objects

```pascal
TObjFunction = record
  Obj: TObj;
  arity: integer;           // parameter count
  upvalueCount: integer;    // number of captured variables
  chunk: pChunk;            // owned bytecode
  name: pObjString;         // nil for the top-level script
end;
```

Each function owns its bytecode chunk (including a constant pool). The top-level script is compiled as a function with `name = nil`.

## Closures and Upvalues

```pascal
TObjClosure = record
  Obj: TObj;
  func: pObjFunction;              // the underlying function
  upvalues: pUpvalueArray;         // array of pObjUpvalue
  upvalueCount: integer;
end;

TObjUpvalue = record
  Obj: TObj;
  location: pValue;                // points to stack slot (when open)
  closed: TValue;                  // holds value after closing
  next: pObjUpvalue;               // linked list of open upvalues
end;
```

Open upvalues point directly into the VM stack. When a local goes out of scope, the upvalue is "closed": the value is copied into `closed` and `location` is redirected to `@closed`.

## Arrays (Extension)

```pascal
TObjArray = record
  Obj: TObj;
  Count: integer;
  Capacity: integer;
  Elements: pValue;        // heap-allocated TValue array
end;
```

Managed via native functions (`newArray`, `arrayPush`, `arrayPop`, `arrayGet`, `arraySet`, `arrayLen`, `arrayRemove`). Elements are traced by the GC.

## Records (Extension)

Two-part design:

```pascal
TObjRecordType = record     // schema
  Obj: TObj;
  name: pObjString;
  fieldCount: integer;
  fieldNames: ppObjString;  // array of interned name pointers
end;

TObjRecord = record         // instance
  Obj: TObj;
  recordType: pObjRecordType;
  fields: pValue;           // flat array, indexed by field position
end;
```

Field lookup is O(n) linear scan of `fieldNames` to find the index, then O(1) access into `fields`. This trades lookup speed for simplicity.

## Native Objects (Extension)

```pascal
TObjNativeObject = record
  Obj: TObj;
  instance: Pointer;              // raw Delphi object (TStringList, etc.)
  classInfo: pNativeClassInfo;    // method table + destructor
end;

TNativeClassInfo = record
  name: AnsiString;
  methods: array of TNativeMethod;
  destructor_: TNativeDestructor;
end;
```

The GC does not trace `instance` (it's an opaque Delphi object). When the `TObjNativeObject` is swept, its `destructor_` is called to free the Delphi object.

## Memory Allocation (`Allocate`)

All tracked allocations go through a single function:

```pascal
procedure Allocate(var p: Pointer; OldSize, NewSize: Integer; MemTracker: pMemTracker);
```

Key behaviors:
1. **GC trigger**: If `NewSize > OldSize` and `BytesAllocated > NextGC`, runs `CollectGarbage`
2. **Stress GC**: Under `{$DEFINE DEBUG_STRESS_GC}`, every growth triggers GC
3. **ReallocMem**: Delegates to Delphi's `ReallocMem` (preserves old content)
4. **Zeroing**: New memory beyond `OldSize` is zeroed via `ClearMem`
5. **Tracking**: `MemTracker.BytesAllocated` is adjusted by `NewSize - OldSize`
6. **Free**: `NewSize = 0` deallocates (pointer becomes nil)

### Array Growth (`AllocateArray`)

```pascal
function AllocateArray(var List: Pointer; var CurrentCapacity: Integer;
  Count, ElemSize: Integer; MemTracker: pMemTracker): Boolean;
```

- Initial allocation: `START_CAPACITY` (256) elements
- Growth: `CurrentCapacity * GROWTH_FACTOR` (2×)
- Returns `True` if reallocation happened
- Overflow guards prevent `Integer` wraparound

### Stack Allocation

The VM stack is **not** tracked by `BytesAllocated`. It uses raw `GetMem`/`ReallocMem`/`FreeMem` to avoid recursive GC triggers during push/pop (which must be safe to call while protecting objects from collection).

## The MemTracker

```pascal
TMemTracker = record
  CreatedObjects: pObj;      // head of intrusive linked list
  BytesAllocated: integer;   // total tracked bytes
  NextGC: integer;           // threshold for next collection
  GrayCount: integer;        // GC worklist count
  GrayCapacity: integer;     // GC worklist capacity
  GrayStack: ^pObj;          // worklist array (raw memory)
  Roots: TRoots;             // root set (stack pointer)
end;
```

Every allocated object is inserted at the head of `CreatedObjects` via `AddToCreatedObjects`. This gives the GC a complete list to sweep.
