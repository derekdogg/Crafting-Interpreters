# Hash Tables & String Interning

## Hash Table Structure

```pascal
TEntry = record
  key:   pObjString;     // nil = empty or tombstone
  value: TValue;         // nil value = truly empty; boolean true = tombstone
end;

TTable = record
  Count:           integer;   // live entries (excludes tombstones)
  CurrentCapacity: integer;   // total slots
  Entries:         pEntry;    // flat array
end;
```

The table uses **open addressing with linear probing**. Deleted entries leave tombstones (key=nil, value=true) to maintain probe chains.

## Hash Function: FNV-1a

```pascal
function HashString(const Key: PAnsiChar; Length: Integer): UInt32;
const
  FNVOffset = 2166136261;
  FNVPrime  = 16777619;
var
  i: Integer;
begin
  Result := FNVOffset;
  for i := 0 to Length - 1 do
  begin
    Result := Result xor UInt8(Key[i]);
    Result := Result * FNVPrime;
  end;
end;
```

FNV-1a is simple, fast, and has good distribution for short strings. Overflow checking is disabled (`{$Q-}`) for this function since wrapping is intentional.

## Operations

### `TableSet`

```pascal
function TableSet(var Table: pTable; key: pObjString; value: TValue;
  MemTracker: pMemTracker): boolean;
```

1. Check load factor: if `Count + 1 > Capacity * 0.75`, grow
2. `FindEntry` to locate the slot
3. If key is nil and value is nil (truly empty): increment `Count`
4. Write key + value
5. Returns `true` if this was a **new** key (used by `OP_SET_GLOBAL` to detect undefined variables)

### `TableGet`

```pascal
function TableGet(Table: pTable; key: pObjString; var value: TValue): boolean;
```

Simple lookup via `FindEntry`. Returns false if not found.

### `TableDelete`

```pascal
function TableDelete(Table: pTable; key: pObjString): boolean;
```

Places a tombstone (key=nil, value=`CreateBoolean(true)`). Does **not** decrement `Count` — tombstones still occupy a slot for probe-chain integrity. They are reclaimed on resize.

### `FindEntry`

```pascal
function FindEntry(Entries: pEntry; Capacity: integer; Key: pObjString): pEntry;
```

Linear probing loop:
- `index := Key.hash mod Capacity`
- If slot is empty (key=nil, value=nil): return (or a previously-found tombstone)
- If slot is tombstone (key=nil, value=true): remember as potential insertion point
- If slot key matches: return
- `index := (index + 1) mod Capacity`

Key comparison uses **pointer identity** (since strings are interned). The only exception is `TableFindString` which compares by content for the interning lookup itself.

## Resizing (`AdjustCapacity`)

```pascal
procedure AdjustCapacity(Table: pTable; NewCapacity: integer; MemTracker: pMemTracker);
```

1. Allocate fresh array of `NewCapacity` entries (all initialized to empty)
2. Rehash all live entries from old array into new
3. Reset `Table.Count` (tombstones are discarded)
4. Free old array

Growth follows the standard pattern: initial 8, then 2× on overflow of `TABLE_MAX_LOAD` (75%).

## String Interning

All strings are interned in `VM.Strings` — a hash table where the **key** is the string and the **value** is nil (the table is used as a hash set).

### Interning on Creation

In `CreateString`:
```pascal
// Check intern table first
Result := TableFindString(VM.Strings, PAnsiChar(S), Len, hash);
if Result <> nil then Exit;    // return existing interned string

// ... allocate new string ...

// Add to intern table (push to protect from GC during TableSet)
pushStack(VM.Stack, StringToValue(Result), VM.MemTracker);
TableSet(VM.Strings, Result, CreateNilValue, MemTracker);
popStack(VM.Stack);
```

### `TableFindString`

```pascal
function TableFindString(Table: pTable; const chars: PAnsiChar;
  length: integer; hash: uint32): pObjString;
```

This is the only table lookup that compares by **content** rather than pointer identity:
1. Probe starting at `hash mod Capacity`
2. For each occupied slot: compare hash, then length, then `CompareMem`
3. Stop at an empty non-tombstone slot

### Benefits of Interning

1. **O(1) string equality**: `ValuesEqual` for strings becomes pointer comparison (via `StringsEqual` which checks length then `CompareMem`, but in practice identical strings share the same pointer)
2. **Reduced memory**: identical strings are stored once
3. **Fast variable lookup**: global variable names (strings) can be compared by identity in the hash table

### GC Integration

Before sweeping, `TableRemoveWhite(VM.Strings)` removes unreferenced interned strings from the set. This ensures the intern table doesn't keep dead strings alive, but also means the intern table can shrink naturally.

## Table Usage in the VM

| Table | Purpose | Key type |
|-------|---------|----------|
| `VM.Strings` | Intern set | string → nil |
| `VM.Globals` | Global variables | string → value |

Both tables share the same implementation. The only difference is how they're used — `Strings` is a set (values are always nil), `Globals` stores arbitrary values.
