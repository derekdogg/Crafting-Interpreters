# Garbage Collector

The GC is a mark-sweep collector with tricolor marking (white/gray/black) and a gray stack worklist. It runs automatically when `BytesAllocated` exceeds `NextGC`, or can be triggered manually via the `collectGarbage()` native function.

## Triggering

Collection is triggered inside `Allocate` when growing memory:

```pascal
if (NewSize > OldSize) then
begin
  {$IFDEF DEBUG_STRESS_GC}
  CollectGarbage;           // stress mode: collect on EVERY allocation
  {$ENDIF}
  if MemTracker.BytesAllocated > MemTracker.NextGC then
    CollectGarbage;         // normal mode: collect at threshold
end;
```

After collection, the threshold is recalculated:
```pascal
VM.MemTracker.NextGC := VM.MemTracker.BytesAllocated * GC_HEAP_GROW_FACTOR;
```
with `GC_HEAP_GROW_FACTOR = 2` and a minimum of 1024 bytes.

## Collection Phases

```pascal
procedure CollectGarbage;
begin
  MarkRoots;
  TraceReferences;
  TableRemoveWhite(VM.Strings);  // sweep intern table
  Sweep;
  // Recalculate NextGC
end;
```

### Phase 1: Mark Roots (`MarkRoots`)

Roots are values the VM can directly reach without traversing object graphs:

1. **Value stack** — every slot from `Values` to `StackTop`
2. **Call frame closures** — `VM.Frames[i].closure` for all active frames
3. **Open upvalue list** — the linked list of unclosed upvalues
4. **Globals table** — all keys and values
5. **Compiler roots** — functions being compiled (via `Current` chain)

```pascal
procedure MarkRoots;
begin
  // Mark the value stack
  slot := VM.Stack.Values;
  while NativeUInt(slot) < NativeUInt(VM.Stack.StackTop) do
  begin
    MarkValue(slot^);
    Inc(slot);
  end;

  // Mark call frame closures
  for i := 0 to VM.FrameCount - 1 do
    MarkObject(pObj(VM.Frames[i].closure));

  // Mark open upvalues
  upvalue := VM.OpenUpvalues;
  while upvalue <> nil do ...

  // Mark globals table
  MarkTable(VM.Globals);

  // Mark compiler roots
  MarkCompilerRoots;
end;
```

### Phase 2: Trace References (`TraceReferences`)

Process the gray stack until empty — each gray object is "blackened" by marking its referenced objects:

```pascal
procedure TraceReferences;
begin
  while VM.MemTracker.GrayCount > 0 do
  begin
    Dec(VM.MemTracker.GrayCount);
    obj := VM.MemTracker.GrayStack[VM.MemTracker.GrayCount];
    BlackenObject(obj);
  end;
end;
```

### `BlackenObject` — per-type tracing

| Object Kind | References Traced |
|-------------|-------------------|
| `okUpvalue` | `closed` value |
| `okFunction` | `name` string, constants array |
| `okClosure` | `func`, all upvalues in the array |
| `okArray` | all `Elements[0..Count-1]` |
| `okRecordType` | `name`, all `fieldNames` |
| `okRecord` | `recordType`, all `fields` |
| `okNative` | (none — leaf) |
| `okString` | (none — leaf) |
| `okNativeObject` | (none — Delphi object is opaque) |

### Phase 3: Remove White Strings (`TableRemoveWhite`)

Before sweeping, remove unmarked strings from the intern table. This prevents dangling pointers in the table after sweep frees the string objects:

```pascal
procedure TableRemoveWhite(Table: pTable);
begin
  for i := 0 to Table.CurrentCapacity - 1 do
    if (entry.key <> nil) and (not entry.key.Obj.IsMarked) then
      TableDelete(Table, entry.key);
end;
```

### Phase 4: Sweep

Walk the `CreatedObjects` linked list. Marked objects survive (mark cleared for next cycle); unmarked objects are freed:

```pascal
procedure Sweep;
begin
  previous := nil;
  obj := VM.MemTracker.CreatedObjects;
  while obj <> nil do
  begin
    if obj^.IsMarked then
    begin
      obj^.IsMarked := false;  // reset for next cycle
      previous := obj;
      obj := obj^.Next;
    end
    else
    begin
      unreached := obj;
      obj := obj^.Next;
      // Unlink from list
      if previous <> nil then
        previous^.Next := obj
      else
        VM.MemTracker.CreatedObjects := obj;
      FreeObject(unreached);
    end;
  end;
end;
```

## Gray Stack

The gray stack is a dynamic array of `pObj` pointers, grown via raw `ReallocMem` (not through `Allocate`, to avoid recursive GC triggers):

```pascal
if VM.MemTracker.GrayCount + 1 > VM.MemTracker.GrayCapacity then
begin
  newCapacity := max(8, GrayCapacity * 2);
  ReallocMem(VM.MemTracker.GrayStack, SizeOf(pObj) * newCapacity);
end;
VM.MemTracker.GrayStack[VM.MemTracker.GrayCount] := obj;
Inc(VM.MemTracker.GrayCount);
```

## `MarkObject` and `MarkValue`

```pascal
procedure MarkObject(obj: pObj);
begin
  if obj = nil then Exit;
  if obj^.IsMarked then Exit;    // already gray or black
  obj^.IsMarked := true;
  // Add to gray stack
  ...
end;

procedure MarkValue(value: TValue);
begin
  if isObject(value) then
    MarkObject(GetObject(value));
end;
```

Only object values need marking — numbers, booleans, and nil are value types.

## GC Safety Patterns

### Protecting Temporaries

When allocating objects that could trigger GC before they're rooted:

```pascal
// Push onto stack to protect from GC during subsequent allocation
pushStack(VM.Stack, StringToValue(strObj), VM.MemTracker);
TableSet(VM.Strings, strObj, CreateNilValue, MemTracker);
popStack(VM.Stack);
```

This pattern appears in:
- `CreateString` (interning new strings)
- `defineNative` (registering native functions)
- `ParseString` (compiling string literals)
- `identifierConstant` (compiler creating name constants)
- `AddValueConstant` (growing the constant pool)
- `functionBody` (creating closures)

### Stack Uses Raw Memory

The VM stack itself is allocated via `GetMem`/`ReallocMem` (not `Allocate`) so that push/pop can never trigger GC. This ensures the "push to protect" pattern is safe.

## `FreeObject`

Dispatches on `ObjectKind`:

| Kind | Cleanup |
|------|---------|
| `okString` | `FreeString` (deallocates via `Allocate(..., 0)`) |
| `okFunction` | Free chunk, then the function record |
| `okClosure` | Free upvalue array, then closure record |
| `okUpvalue` | Free record |
| `okArray` | Free elements array, then array record |
| `okRecordType` | Free fieldNames array, then record |
| `okRecord` | Free fields array, then record |
| `okNativeObject` | Call `destructor_`, then free record |
| `okNative` | Free record |

## Debug Logging

Under `{$DEFINE DEBUG_LOG_GC}`, all allocations, frees, marks, and blackens are logged to `gc.log`. At VM shutdown, a summary reports:
- Total GC cycles
- Allocations vs frees (leak detection)
- Marks vs blackens (trace consistency)

## Stress Testing

Under `{$DEFINE DEBUG_STRESS_GC}`, every allocation triggers a full collection cycle. This makes GC bugs surface immediately at the cost of extreme slowdown. The test suite runs with this enabled to verify correctness.
