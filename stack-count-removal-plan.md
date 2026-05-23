# Stack.Count Removal Plan

## Goal
Remove redundant `Count` field from `TStack`. `StackTop - Values` already encodes the count via pointer arithmetic. Eliminating `Count` saves one `Inc`/`Dec` per push/pop in the hot dispatch loop.

## Current State
```pascal
TStack = record
  Count             : Integer;
  CurrentCapacity   : Integer;
  Values            : pValue;
  StackTop          : pValue;
end;
```

`Count` is always `(NativeUInt(StackTop) - NativeUInt(Values)) div SizeOf(TValue)`.  
Proof: OP_RETURN already computes this exact expression (line ~4298).

## Inline Helper (add before first use)
```pascal
function StackCount(Stack: pStack): Integer; inline;
begin
  Result := (NativeUInt(Stack.StackTop) - NativeUInt(Stack.Values)) div SizeOf(TValue);
end;
```

## Chunks

### Chunk 1: Hot-path pushStack/popStack
**Files**: pushStack, pushStackGrow, popStack  
**Changes**:
- `pushStack` capacity check: `Stack.Count >= Stack.CurrentCapacity` → `Stack.StackTop >= Stack.Values + Stack.CurrentCapacity`
- Remove `Inc(Stack.Count)` from pushStack (line ~1872, ~1895)
- Remove `Dec(Stack.Count)` from popStack (line ~5067)

### Chunk 2: Dispatch loop inline pops (~15 sites)
All `Dec(vm.Stack.Count)` and `Dec(vm.Stack.Count, N)` in Run:
- OP_EQUAL, OP_GREATER, OP_LESS (lines ~3991, 4008, 4022)
- OP_ADD numeric fast-path (line ~4033)
- OP_SUBTRACT, OP_MULTIPLY, OP_DIVIDE, OP_MODULO (lines ~4057, 4070, 4083, 4099)
- OP_POP (line ~4118)
- OP_POP_N (line ~4128)
- OP_DEFINE_GLOBAL (line ~4209)

Just delete the `Dec(vm.Stack.Count)` / `Dec(vm.Stack.Count, N)` lines.

### Chunk 3: CallValue / OP_INVOKE native-pop sites (~6 sites)
Pattern: `VM.Stack.Count := VM.Stack.Count - (argCnt + 1);`  
Delete these — StackTop is already adjusted on the preceding line.
- CallValue native path (line ~3884)
- CallValue record path (line ~3909)
- OP_INVOKE dict (line ~4556)
- OP_INVOKE native obj (line ~4590)
- OP_INVOKE_LONG dict (line ~4622)
- OP_INVOKE_LONG native obj (line ~4654)

### Chunk 4: peekStack
```pascal
// Before:
Result := Stack.Values[Stack.Count - 1 - DistanceFromTop];
// After:
Result := (Stack.StackTop - 1 - DistanceFromTop)^;
```

### Chunk 5: Record field copy in CallValue
```pascal
// Before:
VM.Stack.Values[VM.Stack.Count - 1 - argCnt + j];
// After:
(VM.Stack.StackTop - 1 - argCnt + j)^;
```

### Chunk 6: ResetStack / initStack
- Remove `Stack.Count := 0` (StackTop := Values already does the job)

### Chunk 7: OP_RETURN recomputation
```pascal
// Before:
VM.Stack.Count := (NativeUInt(VM.Stack.StackTop) - NativeUInt(VM.Stack.Values)) div SizeOf(TValue);
// After: delete the line entirely — no Count to update
```

### Chunk 8: Assertions
All `Assert(Stack.Count ...)` sites. Two options:
- A) Replace `Stack.Count` with `StackCount(Stack)` in assert expressions
- B) Wrap in `{$IFDEF DEBUG}` blocks that compute count only when needed

Sites (~12):
- AssertStackIsNotEmpty: `Stack.Count > 0` → `Stack.StackTop > Stack.Values`
- AssertStackTopCountConsistent: entire procedure becomes a no-op or validates StackTop >= Values
- Concatenate exit asserts: use StackCount helper
- OP_POP_N underflow: `vm.Stack.Count >= argCount` → `StackCount(vm.Stack) >= argCount`  
  (or: `vm.Stack.StackTop - vm.Stack.Values >= argCount`)
- Various other sites in introspection, GC, etc.

### Chunk 9: Other readers
- `VM.Stack.Count > 0` guards in error reporting (lines ~5335, 5569) → `VM.Stack.StackTop > VM.Stack.Values`
- `stackBase := VM.Stack.Count` (line ~7491) → `stackBase := StackCount(VM.Stack)`
- `while VM.Stack.Count > stackBase` (line ~7508) → `while StackCount(VM.Stack) > stackBase`
- Dec in OP_RECORD/OP_RECORD_LONG bulk pops (lines ~4982, 5005)

### Chunk 10: Remove the field
Delete `Count : Integer;` from `TStack` record definition.

## Risks
- NativeObjectTest.dpr or other units may reference Stack.Count — grep all .pas files
- Assertions that compute StackCount add a div in debug builds (acceptable)

## Expected Gain
- One fewer memory store per push and per pop
- fibIter loop body: ~8 pushes+pops per iteration → 8 fewer stores × 10M iterations = 80M stores saved
- Likely 3-8% on iterative benchmarks
