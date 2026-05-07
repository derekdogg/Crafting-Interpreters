# Dictionary & Table Implementation Review

This document provides a complete, in-depth review of how hash tables and dictionaries are implemented in the VM as of commit `28557bb`.

---

## Table of Contents

1. [Overview — Two Distinct Table Systems](#1-overview--two-distinct-table-systems)
2. [Type Definitions](#2-type-definitions)
3. [Hash Functions](#3-hash-functions)
4. [TTable — Internal Hash Table](#4-ttable--internal-hash-table)
5. [TObjDictionary — User-Facing Dictionary](#5-tobjdictionary--user-facing-dictionary)
6. [Probing Strategy](#6-probing-strategy)
7. [Tombstone Handling](#7-tombstone-handling)
8. [Growth & Resizing](#8-growth--resizing)
9. [String Interning via TTable](#9-string-interning-via-ttable)
10. [Global Variables via TTable](#10-global-variables-via-ttable)
11. [Dictionary Opcodes & Compilation](#11-dictionary-opcodes--compilation)
12. [Dictionary Methods & Native Functions](#12-dictionary-methods--native-functions)
13. [GC Integration](#13-gc-integration)
14. [Design Comparison: TTable vs TObjDictionary](#14-design-comparison-ttable-vs-tobjdictionary)
15. [Constants & Tuning Parameters](#15-constants--tuning-parameters)

---

## 1. Overview — Two Distinct Table Systems

The VM contains **two separate hash table implementations** that serve different purposes:

| System | Type | Purpose | Key Type | Location |
|--------|------|---------|----------|----------|
| **TTable** | Internal VM infrastructure | String interning (`VM.Strings`) and global variable storage (`VM.Globals`) | `pObjString` only | Lines 461–470 |
| **TObjDictionary** | User-facing language object | First-class dictionary values in Lox programs | Any `TValue` (numbers, booleans, strings, nil, objects) | Lines 340–347 |

Both use **open addressing with linear probing** and share the same load factor (75%) and growth factor (2×), but differ significantly in their internal structure, key comparison strategy, and tombstone management.

---

## 2. Type Definitions

### Constants (Lines 13–20)

```pascal
const
  START_CAPACITY =  16;
  MAX_SIZE       =  MaxInt div 2;
  GROWTH_FACTOR  =  2;
  GC_HEAP_GROW_FACTOR = 2;
  TABLE_MAX_LOAD = 0.75;
```

### TTable System (Lines 461–470)

```pascal
TEntry = record       // Line 461–464
  key     : pObjString;
  value   : TValue;
end;

TTable = record        // Line 466–470
  Count           : integer;
  CurrentCapacity : integer;
  Entries         : pEntry;
end;
```

- `Count` tracks **live entries + tombstones** (tombstones are not subtracted on deletion).
- `Entries` is a dynamically allocated flat array of `TEntry`.
- Keys are always `pObjString` pointers (interned strings).

### TObjDictionary System (Lines 333–347)

```pascal
TDictEntry = record    // Line 333–338
  key       : TValue;
  value     : TValue;
  occupied  : boolean;  // true if slot holds a live entry
  tombstone : boolean;  // true if slot is a deleted entry
end;

TObjDictionary = record  // Line 340–347
  Obj        : TObj;        // GC header (ObjectKind, IsMarked, Next)
  Count      : integer;     // live entries only (excludes tombstones)
  Tombstones : integer;     // separate tombstone counter
  Capacity   : integer;     // total slots in Entries array
  Entries    : pDictEntry;
end;
```

- `Count` tracks **live entries only**. `Tombstones` is a separate field.
- Has explicit `occupied` and `tombstone` booleans per slot (clearer than the TTable encoding).
- Keys can be any `TValue` — numbers, booleans, strings, nil, or object references.
- Inherits from `TObj` so it participates in garbage collection.

### Pointer Types (Lines 200–210)

```pascal
pObjDictionary  = ^TObjDictionary;
pDictEntry      = ^TDictEntry;
pEntry          = ^TEntry;
pTable          = ^TTable;
```

### Object Kind Enum (Line 138)

```pascal
TObjectKind = (okString, okFunction, okNative, okClosure, okUpvalue,
               okArray, okRecordType, okRecord, okNativeObject, okDictionary);
```

`okDictionary` is the tag for dictionary objects in the GC object list.

---

## 3. Hash Functions

### FNV-1a for Strings (Lines 8460–8479)

```pascal
function HashString(const Key: PAnsiChar; Length: Integer): UInt32;
const
  FNVOffset = 2166136261;
  FNVPrime  = 16777619;
var
  i: Integer;
begin
  {$Q-}  // disable overflow checking — wrapping is intentional
  Result := FNVOffset;
  for i := 0 to Length - 1 do
  begin
    Result := Result xor UInt8(Key[i]);
    Result := Result * FNVPrime;
  end;
  {$Q+}
end;
```

- Standard FNV-1a with 32-bit hash.
- Called once when a string is created; the result is cached in `pObjString.hash`.
- `{$Q-}/{$Q+}` disables/enables overflow checking since wrapping is intentional in the hash.

### HashValue for Any TValue (Lines 1860–1888)

```pascal
function HashValue(const value : TValue) : UInt32;
var
  bits : UInt64;
begin
  case value.ValueKind of
    vkNumber: begin
      Move(value.NumberValue, bits, SizeOf(Double));
      Result := UInt32(bits xor (bits shr 32));
    end;
    vkBoolean: begin
      if value.BooleanValue then Result := 1 else Result := 0;
    end;
    vkNull: begin
      Result := 2;
    end;
    vkObject: begin
      case value.ObjValue^.ObjectKind of
        okString: Result := pObjString(value.ObjValue)^.hash;  // reuse cached hash
      else
        Result := UInt32(NativeUInt(value.ObjValue));  // identity hash (pointer address)
      end;
    end;
  else
    Result := 0;
  end;
end;
```

Used by `TObjDictionary` since dictionary keys can be any type.

**Notable behaviors:**
- **Numbers**: Bit-casts the `Double` to `UInt64`, then folds the upper 32 bits into the lower 32 bits via XOR.
- **Booleans**: Maps to 0 or 1.
- **Nil**: Always hashes to 2.
- **Strings**: Reuses the cached FNV-1a hash.
- **Other objects**: Uses the pointer address cast to `UInt32` (identity hash). This means two structurally identical arrays/records at different addresses will hash differently.

---

## 4. TTable — Internal Hash Table

### InitTable (Lines 8214–8228)

```pascal
procedure InitTable(var Table : pTable; memTracker : pMemTracker);
begin
  Allocate(pointer(Table), 0, Sizeof(TTable), MemTracker);
  Table.Count := 0;
  Table.CurrentCapacity := 0;
  Table.Entries := nil;
end;
```

Allocates the `TTable` record itself. The `Entries` array starts as `nil` with zero capacity — it grows on first insertion.

### FindEntry (Lines 8240–8285)

```pascal
function FindEntry(Entries: pEntry; Capacity: integer; Key: pObjString): pEntry;
var
  index: uint32;
  tombstone: pEntry;
  entry: pEntry;
begin
  index := Key.hash mod uint32(Capacity);
  tombstone := nil;
  while true do
  begin
    entry := @Entries[index];
    if entry.key = nil then
    begin
      if isNill(entry.value) then
      begin
        // Empty slot: return tombstone if we passed one, else this empty slot
        if tombstone <> nil then
          Result := tombstone
        else
          Result := entry;
        Exit;
      end
      else
      begin
        // Tombstone (key=nil, value=bool(true)): remember first one
        if tombstone = nil then
          tombstone := entry;
      end;
    end
    else if entry.key = key then
    begin
      // Found the key — pointer identity (strings are interned)
      Result := entry;
      Exit;
    end;
    index := (index + 1) mod uint32(Capacity);
  end;
end;
```

**Key comparison**: Uses **pointer identity** (`entry.key = key`). This works because all strings are interned — two strings with the same content always share the same `pObjString` pointer.

**Slot states** (TTable encoding):
| `key` | `value` | Meaning |
|-------|---------|---------|
| non-nil | any | Live entry |
| nil | nil (vkNull) | Empty slot (never used or cleared by rehash) |
| nil | bool(true) | Tombstone (deleted entry) |

### AdjustCapacity (Lines 8287–8336)

```pascal
procedure AdjustCapacity(Table: pTable; NewCapacity: integer; MemTracker: pMemTracker);
var
  NewEntries, OldEntries: pEntry;
  OldCapacity, i: integer;
  dest: pEntry;
begin
  // Allocate fresh array
  NewEntries := nil;
  Allocate(pointer(NewEntries), 0, NewCapacity * SizeOf(TEntry), MemTracker);

  // Initialize all slots to empty (key=nil, value=nil)
  for i := 0 to NewCapacity - 1 do
  begin
    NewEntries[i].key := nil;
    NewEntries[i].value := CreateNilValue;
  end;

  // Rehash existing live entries (tombstones are discarded)
  OldEntries := Table.Entries;
  OldCapacity := Table.CurrentCapacity;
  Table.Count := 0;   // reset count; re-incremented below

  if OldEntries <> nil then
  begin
    for i := 0 to OldCapacity - 1 do
    begin
      if OldEntries[i].key = nil then Continue;  // skip empty + tombstones
      dest := FindEntry(NewEntries, NewCapacity, OldEntries[i].key);
      dest.key := OldEntries[i].key;
      dest.value := OldEntries[i].value;
      Inc(Table.Count);
    end;
    Allocate(pointer(OldEntries), OldCapacity * SizeOf(TEntry), 0, MemTracker);
  end;

  Table.Entries := NewEntries;
  Table.CurrentCapacity := NewCapacity;
end;
```

- Allocates a completely new array, rehashes all live entries, and frees the old array.
- **Tombstones are discarded** during rehash (any entry with `key = nil` is skipped).
- `Count` is reset to 0 and re-counted from live entries only during rehash.

### TableSet (Lines 8338–8367)

```pascal
function TableSet(var Table: pTable; key: pObjString; value: TValue;
  MemTracker: pMemTracker): boolean;
var
  Entry: pEntry;
  NewCapacity: integer;
begin
  // Grow if needed: (count + 1) > capacity * 0.75
  if (Table.Count + 1 > Table.CurrentCapacity * TABLE_MAX_LOAD) then
  begin
    if Table.CurrentCapacity < 8 then
      NewCapacity := 8
    else
      NewCapacity := Table.CurrentCapacity * GROWTH_FACTOR;
    AdjustCapacity(Table, NewCapacity, MemTracker);
  end;

  Entry := FindEntry(Table.Entries, Table.CurrentCapacity, key);
  Result := Entry.key = nil;           // true = this is a new key
  if Result and isNill(Entry.value) then
    Inc(Table.Count);                  // only increment for truly empty (not tombstone)

  Entry.key := key;
  Entry.value := value;
end;
```

**Returns `true`** if the key was new (not previously in the table).

**Count behavior**: `Count` is only incremented when placing into a truly empty slot (not a tombstone). This means `Count` tracks `live entries + tombstones` since tombstones don't decrement `Count` on deletion. This is the book's original design — tombstones count toward the load factor threshold.

### TableGet (Lines 8426–8443)

```pascal
function TableGet(Table: pTable; key: pObjString; var value: TValue): boolean;
var
  Entry: pEntry;
begin
  if Table.Count = 0 then Exit(false);

  Entry := FindEntry(Table.Entries, Table.CurrentCapacity, key);
  if Entry.key = nil then Exit(false);

  value := Entry.value;
  Result := true;
end;
```

### TableDelete (Lines 8370–8390)

```pascal
function TableDelete(Table: pTable; key: pObjString): boolean;
var
  Entry: pEntry;
begin
  if Table.Count = 0 then Exit(false);

  Entry := FindEntry(Table.Entries, Table.CurrentCapacity, key);
  if Entry.key = nil then Exit(false);

  // Place a tombstone: key=nil, value=bool(true)
  Entry.key := nil;
  Entry.value := CreateBoolean(true);
  Result := true;
end;
```

**`Count` is NOT decremented.** The tombstone continues to count toward the load factor. This means heavy delete-insert cycles without resizing can waste slots. The tombstone is only reclaimed when `AdjustCapacity` rehashes the table.

### TableFindString (Lines 8392–8424)

```pascal
function TableFindString(Table: pTable; const chars: PAnsiChar;
  length: integer; hash: uint32): pObjString;
var
  index: uint32;
  entry: pEntry;
begin
  Result := nil;
  if Table.Count = 0 then Exit;

  index := hash mod uint32(Table.CurrentCapacity);
  while true do
  begin
    entry := @Table.Entries[index];
    if entry.key = nil then
    begin
      // Stop at empty non-tombstone slot
      if isNill(entry.value) then Exit;
    end
    else if (entry.key.length = length) and
            (entry.key.hash = hash) and
            CompareMem(@entry.key.chars[0], chars, length) then
    begin
      Result := entry.key;
      Exit;
    end;
    index := (index + 1) mod uint32(Table.CurrentCapacity);
  end;
end;
```

**This is the only function that compares strings by content** rather than pointer identity. It's used exclusively for string interning — when creating a new string, we need to check if an identical string already exists. The check is: same length → same hash → byte-for-byte `CompareMem`.

### FreeTable (Lines 8444–8458)

```pascal
procedure FreeTable(var Table : pTable; memTracker : pMemTracker);
begin
  if Table.Entries <> nil then
    FreeEntries(Table.Entries, Table.CurrentCapacity, MemTracker);
  Allocate(pointer(Table), Sizeof(TTable), 0, MemTracker);
  Table := nil;
end;
```

### FreeEntries (Lines 8230–8238)

```pascal
procedure FreeEntries(var Entries : pEntry; Capacity : integer; MemTracker : pMemTracker);
begin
  Allocate(pointer(Entries), Capacity * Sizeof(TEntry), 0, MemTracker);
end;
```

---

## 5. TObjDictionary — User-Facing Dictionary

### newDictionary (Lines 1839–1853)

```pascal
function newDictionary(MemTracker : pMemTracker) : pObjDictionary;
begin
  Result := nil;
  Allocate(Pointer(Result), 0, SizeOf(TObjDictionary), MemTracker);
  Result^.Obj.ObjectKind := okDictionary;
  Result^.Obj.IsMarked := false;
  Result^.Obj.Next := nil;
  Result^.Count := 0;
  Result^.Tombstones := 0;
  Result^.Capacity := 0;
  Result^.Entries := nil;
  AddToCreatedObjects(pObj(Result), MemTracker);
end;
```

Adds itself to the GC object list via `AddToCreatedObjects`.

### DictFindEntry (Lines 1890–1932)

```pascal
function DictFindEntry(entries : pDictEntry; capacity : integer;
  const key : TValue) : pDictEntry;
var
  index : UInt32;
  entry : pDictEntry;
  tombstone : pDictEntry;
  probeCount : integer;
begin
  index := HashValue(key) mod UInt32(capacity);
  tombstone := nil;
  for probeCount := 0 to capacity - 1 do
  begin
    entry := @entries[index];
    if not entry^.occupied and not entry^.tombstone then
    begin
      // Empty slot
      if tombstone <> nil then Result := tombstone
      else Result := entry;
      Exit;
    end
    else if entry^.tombstone then
    begin
      if tombstone = nil then tombstone := entry;
    end
    else if ValuesEqual(entry^.key, key) then
    begin
      Result := entry;
      Exit;
    end;
    index := (index + 1) mod UInt32(capacity);
  end;
  Assert(false, 'DictFindEntry: probed entire table without finding slot');
  Result := nil;
end;
```

**Key comparison**: Uses `ValuesEqual` which does deep value comparison (not pointer identity). This is necessary because dictionary keys can be any `TValue`.

**Safety**: Uses a bounded `for` loop (`probeCount := 0 to capacity - 1`) instead of `while true`. If the entire table is probed without finding a slot, an assertion fires. This prevents infinite loops.

**Slot states** (TObjDictionary encoding):
| `occupied` | `tombstone` | Meaning |
|------------|-------------|---------|
| `true` | `false` | Live entry |
| `false` | `false` | Empty slot (never used) |
| `false` | `true` | Tombstone (deleted entry) |
| `true` | `true` | Invalid (should never occur) |

### DictGrow (Lines 1934–1991)

```pascal
procedure DictGrow(dict : pObjDictionary; MemTracker : pMemTracker);
var
  newCapacity, i : integer;
  newEntries : pDictEntry;
  entry, dest : pDictEntry;
begin
  if dict^.Capacity < 8 then newCapacity := 8
  else newCapacity := dict^.Capacity * GROWTH_FACTOR;

  newEntries := nil;
  Allocate(Pointer(newEntries), 0, newCapacity * SizeOf(TDictEntry), MemTracker);

  // Note: Allocate zero-fills memory, so occupied=false and tombstone=false by default

  dict^.Count := 0;
  for i := 0 to dict^.Capacity - 1 do
  begin
    entry := @dict^.Entries[i];
    if not entry^.occupied then Continue;  // skip empty and tombstones
    dest := DictFindEntry(newEntries, newCapacity, entry^.key);
    dest^.key := entry^.key;
    dest^.value := entry^.value;
    dest^.occupied := true;
    dest^.tombstone := false;
    Inc(dict^.Count);
  end;

  if dict^.Entries <> nil then
    Allocate(Pointer(dict^.Entries), dict^.Capacity * SizeOf(TDictEntry), 0, MemTracker);
  dict^.Entries := newEntries;
  dict^.Capacity := newCapacity;
  dict^.Tombstones := 0;  // all tombstones discarded
end;
```

### DictSet (Lines 2025–2063)

```pascal
procedure DictSet(dict : pObjDictionary; const key : TValue; const val : TValue;
  MemTracker : pMemTracker);
var
  entry : pDictEntry;
  isNewKey : boolean;
begin
  // Grow if (count + tombstones + 1) > capacity * 0.75
  if dict^.Count + dict^.Tombstones + 1 > Integer(Trunc(dict^.Capacity * TABLE_MAX_LOAD)) then
    DictGrow(dict, MemTracker);

  entry := DictFindEntry(dict^.Entries, dict^.Capacity, key);
  isNewKey := not entry^.occupied;
  if isNewKey then
  begin
    if entry^.tombstone then
      Dec(dict^.Tombstones);  // reusing a tombstone slot
    Inc(dict^.Count);
  end;
  entry^.key := key;
  entry^.value := val;
  entry^.occupied := true;
  entry^.tombstone := false;
end;
```

**Load factor calculation**: Uses `Count + Tombstones` (not just `Count`). This is more accurate than TTable's approach since it considers the actual number of occupied slots.

**Tombstone reuse**: When inserting into a tombstone slot, decrements `Tombstones` and increments `Count`, keeping both counters accurate.

### DictGet (Lines 1993–2023)

```pascal
function DictGet(dict : pObjDictionary; const key : TValue; var outValue : TValue) : boolean;
var
  entry : pDictEntry;
begin
  if dict^.Count = 0 then Exit(false);

  entry := DictFindEntry(dict^.Entries, dict^.Capacity, key);
  if not entry^.occupied then Exit(false);

  outValue := entry^.value;
  Result := true;
end;
```

### DictDelete (Lines 2064–2100)

```pascal
function DictDelete(dict : pObjDictionary; const key : TValue) : boolean;
var
  entry : pDictEntry;
begin
  if dict^.Count = 0 then Exit(false);

  entry := DictFindEntry(dict^.Entries, dict^.Capacity, key);
  if not entry^.occupied then Exit(false);

  // Place tombstone
  entry^.occupied := false;
  entry^.tombstone := true;
  Dec(dict^.Count);
  Inc(dict^.Tombstones);
  Result := true;
end;
```

**Unlike TTable**, `Count` IS decremented and `Tombstones` IS incremented. This gives accurate counts at all times.

---

## 6. Probing Strategy

Both tables use **open addressing with linear probing**:

```
index := hash mod capacity
// on collision:
index := (index + 1) mod capacity
```

### How Lookups Work

1. Compute the initial bucket: `hash mod capacity`.
2. Check the slot:
   - **Live entry with matching key** → found, return it.
   - **Empty slot (never used)** → key is not in the table. Return the first tombstone encountered (if any) for potential reuse, otherwise return this empty slot.
   - **Tombstone** → remember it as a candidate reuse slot, continue probing.
   - **Live entry with different key** → collision, advance to next slot.
3. Repeat step 2 at `(index + 1) mod capacity`.

### Tombstone Reuse During Insert

When `FindEntry`/`DictFindEntry` is called for an insert and the key doesn't exist, it returns the **first tombstone** encountered during the probe (if any). This reclaims tombstone slots for new entries, keeping the table dense.

---

## 7. Tombstone Handling

### TTable Tombstones

- **Encoding**: `key = nil`, `value = CreateBoolean(true)`
- **On delete**: Set tombstone, `Count` unchanged
- **On insert to tombstone slot**: `Count` unchanged (checked via `isNill(entry.value)`)
- **Load factor**: `Count` includes tombstones, so they contribute to triggering growth
- **Cleanup**: Only during `AdjustCapacity` (full rehash)
- **Risk**: Heavy delete-without-insert patterns can accumulate tombstones without triggering a resize, degrading probe chain lengths

### TObjDictionary Tombstones

- **Encoding**: `occupied = false`, `tombstone = true`
- **On delete**: `Count` decremented, `Tombstones` incremented
- **On insert to tombstone slot**: `Tombstones` decremented, `Count` incremented
- **Load factor**: Uses `Count + Tombstones` explicitly
- **Cleanup**: During `DictGrow` (full rehash), `Tombstones` reset to 0
- **Advantage**: Accurate count tracking, clearer semantics

---

## 8. Growth & Resizing

### Parameters

| Parameter | Value |
|-----------|-------|
| Load factor threshold | 75% (`TABLE_MAX_LOAD = 0.75`) |
| Growth factor | 2× (`GROWTH_FACTOR = 2`) |
| Minimum capacity | 8 slots |

### TTable Growth Trigger (in TableSet)

```pascal
if (Table.Count + 1 > Table.CurrentCapacity * TABLE_MAX_LOAD) then
```

Since `Count` includes tombstones, frequent deletions inflate `Count` and may trigger growth earlier than necessary. However, growth discards tombstones, so this is self-correcting.

### TObjDictionary Growth Trigger (in DictSet)

```pascal
if dict^.Count + dict^.Tombstones + 1 > Integer(Trunc(dict^.Capacity * TABLE_MAX_LOAD)) then
```

Uses the actual occupied slot count (`Count + Tombstones`), which is more precise.

### Resize Process (both tables)

1. Allocate a new array of `newCapacity` slots, initialized to empty.
2. Iterate the old array. For each live entry (non-nil key / occupied=true), rehash it into the new array via `FindEntry`/`DictFindEntry`.
3. Free the old array.
4. Reset count from the rehash (only live entries survive).

Tombstones are **not** copied — they are discarded during rehash.

---

## 9. String Interning via TTable

`VM.Strings` is a `pTable` used as a **hash set** (values are always `CreateNilValue`).

### How Interning Works (CreateString, Lines 1367–1416)

```
1. Compute hash = HashString(chars, length)
2. Look up in VM.Strings via TableFindString (content comparison)
3. If found → return existing pObjString (no allocation)
4. If not found:
   a. Allocate new pObjString with the hash cached
   b. Push to stack (GC protection)
   c. TableSet(VM.Strings, newString, nil)  — add to intern set
   d. Pop from stack
   e. Return new string
```

### Why This Matters

Because all strings are interned:
- **String equality** reduces to **pointer comparison** (`str1 = str2`).
- `FindEntry` can compare keys by pointer identity instead of content comparison.
- `TableFindString` is the **only** function that does content-based string comparison — and it's used exclusively during interning.

### GC Interaction

Interned strings that are no longer referenced must be removed from `VM.Strings` before sweep. See [GC Integration](#13-gc-integration).

---

## 10. Global Variables via TTable

`VM.Globals` is a `pTable` where keys are interned variable name strings and values are the variable's current `TValue`.

### Opcodes

| Opcode | Operand | Behavior |
|--------|---------|----------|
| `OP_DEFINE_GLOBAL` (1-byte index) | Constant index → string name | `TableSet(Globals, name, peek())`, then pop |
| `OP_GET_GLOBAL` (1-byte index) | Constant index → string name | `TableGet(Globals, name)` → push result; runtime error if undefined |
| `OP_SET_GLOBAL` (1-byte index) | Constant index → string name | `TableSet(Globals, name, peek())`; if key was new → undo + runtime error (variable didn't exist) |
| `OP_DEFINE_GLOBAL_LONG` (3-byte index) | Same but with 24-bit constant index | Same behavior as above |
| `OP_GET_GLOBAL_LONG` (3-byte index) | Same | Same |
| `OP_SET_GLOBAL_LONG` (3-byte index) | Same | Same |

### OP_SET_GLOBAL — The "Undo" Pattern (Lines 3187–3198)

```pascal
OP_SET_GLOBAL: begin
  value := ReadConstantFr;
  if TableSet(vm.Globals, ValueToString(value), peekStack(vm.Stack), vm.MemTracker) then
  begin
    // TableSet returned true → key was new → variable doesn't exist
    TableDelete(vm.Globals, ValueToString(value));   // undo the insertion
    runtimeError('Undefined variable ...');
    exit;
  end;
end;
```

`TableSet` returns `true` when the key is new. For `OP_SET_GLOBAL`, a new key means the variable was never defined, which is a runtime error. The just-inserted entry is immediately deleted.

---

## 11. Dictionary Opcodes & Compilation

### OP_DICT_LITERAL (Opcode 45)

**Compiler** (Lines 7850, 7883): Emitted when parsing a dictionary literal `{k1: v1, k2: v2}`.

```pascal
emitByte(OP_DICT_LITERAL, CurrentChunk, parser.previous.line, vm.MemTracker);
```

**Operand**: 1-byte pair count (max 255 key-value pairs in a literal).

**VM execution** (Lines 3760–3787):

```
Stack before: [... key0, val0, key1, val1, ...]
  1. Read pair count from bytecode
  2. Create new dictionary object
  3. Push dict onto stack (GC safety)
  4. For each pair, read key/value from below dict on stack
  5. DictSet each pair into the dictionary
  6. Pop dict, pop all key-value pairs, push dict back
Stack after:  [... dict]
```

---

## 12. Dictionary Methods & Native Functions

### Method Dispatch (InvokeDictMethod, Lines 2725–2855)

When a method is called on a dictionary object (e.g., `myDict.Set("key", 42)`), `InvokeDictMethod` dispatches to the appropriate operation:

| Method | Args | Description |
|--------|------|-------------|
| `.Set(key, value)` | 2 | Calls `DictSet`. Returns the value. |
| `.Get(key)` | 1 | Calls `DictGet`. Runtime error if key not found. |
| `.Has(key)` | 1 | Returns boolean: whether key exists. |
| `.Delete(key)` | 1 | Calls `DictDelete`. Returns boolean: whether key was found. |
| `.Keys()` | 0 | Returns an array of all live keys. |
| `.Values()` | 0 | Returns an array of all live values. |
| `.Size()` | 0 | Returns the live entry count as a number. |

### Native Functions (Lines 5584–5790)

Standalone function equivalents registered as globals in `InitVM`:

- `dictNew()` → `newDictionary`
- `dictSet(dict, key, value)` → `DictSet`
- `dictGet(dict, key)` → `DictGet`
- `dictHas(dict, key)` → boolean
- `dictDelete(dict, key)` → boolean
- `dictKeys(dict)` → array
- `dictValues(dict)` → array
- `dictSize(dict)` → number

---

## 13. GC Integration

### Marking

**MarkTable** (Lines 4212–4225) — marks all keys and values in a `TTable`:

```pascal
procedure MarkTable(Table : pTable);
begin
  if Table = nil then Exit;
  if Table^.Entries = nil then Exit;
  for i := 0 to Table^.CurrentCapacity - 1 do
  begin
    if Table^.Entries[i].key <> nil then
    begin
      MarkObject(pObj(Table^.Entries[i].key));
      MarkValue(Table^.Entries[i].value);
    end;
  end;
end;
```

Called for `VM.Globals` during the mark phase to keep global variable names and values alive.

**Dictionary marking**: When a `TObjDictionary` is encountered during tracing, its entries' keys and values are marked (handled in the generic object marking logic).

### Sweep — TableRemoveWhite (Lines 4413–4425)

```pascal
procedure TableRemoveWhite(Table : pTable);
begin
  if Table = nil then Exit;
  if Table^.Entries = nil then Exit;
  for i := 0 to Table^.CurrentCapacity - 1 do
  begin
    if (Table^.Entries[i].key <> nil) and
       (not Table^.Entries[i].key^.Obj.IsMarked) then
      TableDelete(Table, Table^.Entries[i].key);
  end;
end;
```

Called on `VM.Strings` (the intern table) **before** sweep. Removes entries whose string keys are unmarked (unreachable). This prevents the intern table from keeping dead strings alive. The deleted entries become tombstones.

### Dictionary Object Freeing

When a `TObjDictionary` is swept (unreachable), its `Entries` array is freed via `Allocate(..., oldSize, 0, ...)`.

---

## 14. Design Comparison: TTable vs TObjDictionary

| Aspect | TTable | TObjDictionary |
|--------|--------|----------------|
| **Purpose** | VM internals (interning, globals) | User-facing dictionary values |
| **Key type** | `pObjString` only | Any `TValue` |
| **Key comparison** | Pointer identity (interned) | `ValuesEqual` (deep comparison) |
| **Hash function** | `key.hash` (cached FNV-1a) | `HashValue(key)` (type-dispatched) |
| **Tombstone encoding** | `key=nil, value=bool(true)` | `occupied=false, tombstone=true` |
| **Count semantics** | `Count` = live + tombstones | `Count` = live only; separate `Tombstones` field |
| **Count on delete** | Not decremented | Decremented |
| **Load factor input** | `Count` (includes tombstones implicitly) | `Count + Tombstones` (explicit) |
| **Probe loop** | `while true` (unbounded) | `for 0 to capacity-1` (bounded with assert) |
| **GC participation** | Not a GC object; VM owns it | GC object (`TObj` header, tracked in object list) |
| **Find-by-content** | `TableFindString` (for interning) | Not needed (uses `ValuesEqual`) |

### Key Observations

1. **TObjDictionary has cleaner tombstone tracking** — separate `Count` and `Tombstones` fields give accurate metrics at all times.
2. **TTable's `Count` is misleading** — it includes tombstones, so `Table.Count` doesn't reflect the actual number of live entries.
3. **TObjDictionary has a bounded probe loop** — safer against infinite loops if the table is somehow full.
4. **TTable relies on string interning** for O(1) key comparison, while TObjDictionary must do full value comparison.

---

## 15. Constants & Tuning Parameters

```pascal
const
  START_CAPACITY      = 16;       // not used by tables (they start at 0 and grow to 8)
  MAX_SIZE            = MaxInt div 2;
  GROWTH_FACTOR       = 2;        // double capacity on resize
  GC_HEAP_GROW_FACTOR = 2;        // GC threshold growth
  TABLE_MAX_LOAD      = 0.75;     // 75% load factor trigger
```

The minimum capacity on first growth is hardcoded to **8** in both `TableSet` and `DictGrow`:

```pascal
if Table.CurrentCapacity < 8 then
  NewCapacity := 8
else
  NewCapacity := Table.CurrentCapacity * GROWTH_FACTOR;
```

Growth sequence: `0 → 8 → 16 → 32 → 64 → 128 → ...`
