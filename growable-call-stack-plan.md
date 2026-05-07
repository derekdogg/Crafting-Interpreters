# Growable Call-Frame Stack

## Problem

The VM has a fixed `FRAMES_MAX = 256` static array of `TCallFrame` inside `TVirtualMachine`. This means:

- Deep recursion (>256 calls) crashes with a hard runtime error
- 256 × sizeof(TCallFrame) is allocated eagerly even for trivial scripts
- Users cannot configure the limit for their use case
- Recursive algorithms (tree walks, divide-and-conquer, interpreters-in-interpreters) are artificially constrained

## Historical Precedent (Young Embedded VMs)

| Language (era) | Call stack | Limit | Configurable? |
|---|---|---|---|
| Lua 4.0 (2000) | Growable `CallInfo*` via realloc, doubling | 200 default | `lua_setcallhook` |
| CPython 1.5 (1997) | Heap-allocated frame objects | 1000 default | `sys.setrecursionlimit(n)` |
| Ruby/YARV (2007) | Growable frame array | ~10000 | `RUBY_THREAD_VM_STACK_SIZE` env |
| Tcl 8.0 (1997) | Heap frames | 1000 default | `interp recursionlimit n` |

All share: **dynamic growth + soft configurable limit**.

## Design

### New Constants

```pascal
FRAMES_INITIAL_CAPACITY = 64;    // Start small (~1.5 KB)
FRAMES_DEFAULT_MAX      = 1024;  // Soft limit (configurable from script)
FRAMES_HARD_MAX         = 65536; // Absolute ceiling (prevents memory exhaustion)
FRAMES_GROWTH_FACTOR    = 2;     // Double on growth (same as value stack)
```

**Why 65536 and not 1M?** Even modest frames become dangerous at scale. If a frame grows to 128 bytes (closures, debug metadata), 1M frames = 128 MB. That's swap death / DoS territory. 65k deep calls is already enormous — well beyond any legitimate recursion that isn't TCO-eligible. Lua stayed conservative for the same reason.

### Type Changes

```pascal
// Before
TVirtualMachine = record
  Frames     : array[0..FRAMES_MAX - 1] of TCallFrame;
  FrameCount : integer;
  ...
end;

// After
TCallFrameArray = array[0..0] of TCallFrame;  // open array for indexed access
pCallFrameArray = ^TCallFrameArray;

TVirtualMachine = record
  Frames        : pCallFrameArray; // heap-allocated, growable
  FrameCount    : integer;         // current depth
  FrameCapacity : integer;         // allocated slots
  MaxCallDepth  : integer;         // soft limit (default 1024)
  ...
end;
```

Using `array[0..0]` (the open-array-on-heap idiom) gives us `VM.Frames^[i]` indexing syntax — safer and clearer than raw pointer arithmetic `(ptr + i)^`. This is the same pattern used by Delphi RTL internals and matches the existing `TUpvalueArray = array[0..UINT8_COUNT-1]` approach in the codebase.

### Growth Logic (in CallValue)

```pascal
// Before: hard error at limit
if VM.FrameCount = FRAMES_MAX then
begin
  runtimeError('Stack overflow (max 256 frames)...');
  Exit(false);
end;

// After: soft limit check, then grow if needed
if VM.FrameCount >= VM.MaxCallDepth then
begin
  runtimeError('Stack overflow (max ' + IntToStr(VM.MaxCallDepth) +
    ' frames). Use setMaxCallDepth() to increase. Last call: ' + ...);
  Exit(false);
end;

if VM.FrameCount >= VM.FrameCapacity then
  GrowFrameStack;  // doubles capacity via ReallocMem
```

### GrowFrameStack Procedure

```pascal
procedure GrowFrameStack;
var
  NewCapacity: integer;
begin
  // WARNING: GrowFrameStack invalidates ALL pCallFrame / ^TCallFrame pointers.
  // Any cached `frame` local in Run is stale after this returns.
  // Re-entrant VM calls are NOT safe across this boundary.
  Assert(VM.FrameCapacity > 0, 'GrowFrameStack: capacity is 0');
  Assert(VM.FrameCapacity <= MaxInt div FRAMES_GROWTH_FACTOR, 'GrowFrameStack: capacity overflow');

  // Cap growth at MaxCallDepth — never allocate beyond the policy limit
  NewCapacity := Min(
    VM.MaxCallDepth,
    VM.FrameCapacity * FRAMES_GROWTH_FACTOR
  );
  // But must grow at least by 1 beyond current count
  if NewCapacity <= VM.FrameCount then
    NewCapacity := VM.FrameCount + 1;

  ReallocMem(VM.Frames, NewCapacity * SizeOf(TCallFrame));
  // Zero new portion (prevents GC from seeing garbage pointers)
  FillChar(VM.Frames^[VM.FrameCapacity],
    (NewCapacity - VM.FrameCapacity) * SizeOf(TCallFrame), 0);
  VM.FrameCapacity := NewCapacity;

  Assert(VM.FrameCapacity > VM.FrameCount, 'GrowFrameStack: capacity did not grow past count');
end;
```

**Growth capping rationale:** If `MaxCallDepth = 1024` and current capacity is 512, doubling gives 1024 — no point allocating 2048 slots you'll never use. Tighter memory footprint.

### Why No Rebase Problem

The value stack had a rebase problem because `frame.slots` holds raw pointers into it. When the value stack moves, those pointers must be fixed.

The frame stack does NOT have this problem because:
1. `Run` re-fetches `frame := @VM.Frames^[VM.FrameCount - 1]` after every `OP_CALL` and `OP_RETURN`
2. Growth only happens inside `CallValue`, which returns to `Run`, which immediately rebases `frame`
3. No other code holds persistent pointers into the frame array

The ONE exception is the `pushStack` rebase loop (line ~1654) which iterates `VM.Frames^[i].slots`. This loop runs when the **value** stack grows — at that point we're mid-push, not mid-call, so `VM.Frames` pointer is stable. Safe.

### Future Architecture: Eliminate Cached `frame` Pointer

The current hot loop in `Run` caches:

```pascal
frame := @VM.Frames^[VM.FrameCount - 1];
```

and reuses `frame^.ip`, `frame^.slots` heavily. This was acceptable with static storage. With movable storage it relies on *discipline* — the programmer must never dereference `frame` after any operation that could grow the frame array.

**Long-term (not this PR):** Replace with an inline accessor:

```pascal
function CurrentFrame: pCallFrame; inline;
begin
  Result := @VM.Frames^[VM.FrameCount - 1];
end;
```

This makes pointer lifetime expression-scoped, eliminates the rebase hazard class entirely, and makes future re-entrancy/coroutine work less terrifying. The performance cost of an inline function is zero in optimized builds.

For **this PR**: keep the cached `frame` pattern (it works, it's tested), add prominent warning comments at every rebase site.

### Memory Policy: Monotonic Growth

**The frame stack grows but never shrinks for the lifetime of the VM.**

Rationale:
- Realloc churn on shrink is worse than holding memory
- A recursive spike followed by shallow execution is common; the memory will be reused on next deep call
- Shrinking would require re-validating that no external code holds frame pointers (hard to guarantee)
- Lua, CPython, YARV all follow this policy

Explicitly documented to prevent future maintainers from "optimizing" with a shrink heuristic.

### New Native Functions

| Function | Signature | Purpose |
|---|---|---|
| `getMaxCallDepth()` | `() -> number` | Returns current soft limit |
| `setMaxCallDepth(n)` | `(number) -> nil` | Sets soft limit (min 64, max 65536) |
| `vmFrameCapacity()` | `() -> number` | Returns currently allocated frame slots |

`setMaxCallDepth` validation:
- `n < 64` → runtime error (too low to be useful)
- `n > FRAMES_HARD_MAX (65536)` → runtime error
- `n < VM.FrameCount` → accepted (Python semantics: takes effect on next call, doesn't unwind)

### Files Changed

Only `Chunk_Types.pas` — all changes are in this single unit.

### All Touch Points

| # | Location | Current Code | Change |
|---|---|---|---|
| 1 | Line 132 | `FRAMES_MAX = 256` | Replace with new constants |
| 2 | Line 133 | `STACK_MAX = FRAMES_MAX * UINT8_COUNT` | Remove (dead code — value stack is already dynamic) |
| 3 | Line 399 | `Frames: array[0..FRAMES_MAX-1] of TCallFrame` | `Frames: pCallFrame` + new fields |
| 4 | ~Line 6391 | `InitVM`: no frame init | Allocate initial frame array, set defaults |
| 5 | ~Line 6507 | `FreeVM`: no frame cleanup | FreeMem the frame array |
| 6 | Line 2975-2978 | Hard overflow check in `CallValue` | Soft limit + grow |
| 7 | Line 2981 | `frame := @VM.Frames[VM.FrameCount]` | `frame := @VM.Frames^[VM.FrameCount]` |
| 8 | Line 1654-1655 | Rebase loop: `VM.Frames[i].slots` | `VM.Frames^[i].slots` |
| 9 | Line 3046 | `frame := @VM.Frames[VM.FrameCount - 1]` | `frame := @VM.Frames^[VM.FrameCount - 1]` |
| 10 | Line 3253 | Same pattern after OP_CALL | Same |
| 11 | Line 3315 | Same pattern after OP_RETURN | Same |
| 12 | Line 4057-4059 | `VM.Frames[0].closure := ...` | `VM.Frames^[0].closure := ...` |
| 13 | Line 4274-4279 | MarkRoots iteration | `VM.Frames^[i]` + additional assertions |
| 14 | Line 4503 | DEBUG_STRESS_GC verify | Same |
| 15 | Native registration | — | Add 3 new native functions |
| 16 | `vmCallDepthNative` | Already exists | No change needed |

### Assertions to Add

```pascal
// GrowFrameStack entry
Assert(VM.FrameCapacity > 0, 'GrowFrameStack: capacity is 0');
Assert(VM.FrameCapacity <= MaxInt div FRAMES_GROWTH_FACTOR, 'GrowFrameStack: capacity overflow');
Assert(VM.Frames <> nil, 'GrowFrameStack: Frames is nil');

// GrowFrameStack exit
Assert(VM.FrameCapacity > VM.FrameCount, 'GrowFrameStack: capacity did not grow past count');
Assert(VM.Frames <> nil, 'GrowFrameStack: Frames is nil after realloc');

// CallValue after growth
Assert(VM.FrameCount < VM.FrameCapacity, 'CallValue: no room after grow');

// InitVM
Assert(VM.Frames <> nil, 'InitVM: Frames allocation failed');
Assert(VM.FrameCapacity = FRAMES_INITIAL_CAPACITY, 'InitVM: FrameCapacity not set');
Assert(VM.MaxCallDepth = FRAMES_DEFAULT_MAX, 'InitVM: MaxCallDepth not set');

// MarkRoots (critical — GC corruption is catastrophic)
Assert(VM.Frames <> nil, 'MarkRoots: Frames is nil');
Assert(VM.FrameCount >= 0, 'MarkRoots: FrameCount is negative');
Assert(VM.FrameCount <= VM.FrameCapacity, 'MarkRoots: FrameCount exceeds FrameCapacity');

// setMaxCallDepth bounds
// (these are runtime errors, not assertions — user-facing)
```

### Testing Strategy

#### Regression

1. **Existing 200+ tests pass** — no behavioral change for scripts using <64 frames

#### Core Functionality

2. **Deep recursion** — tail-call counter to 1000+ (verifies growth works)
3. **Limit enforcement** — overflow at default 1024, get error, call `setMaxCallDepth(2048)`, succeed
4. **Introspection** — verify `vmFrameCapacity()` grows as calls deepen
5. **`setMaxCallDepth` below current depth** — set limit low while deep, verify next call fails
6. **`setMaxCallDepth` bounds** — verify rejection of <64 and >65536

#### Realloc Boundary Tests (Critical)

7. **63 → 64 crossing** — recursion that hits exactly the initial capacity boundary
8. **64 → 65 crossing** — first growth event; stale-pointer bugs appear here
9. **127 → 128 crossing** — second growth event; verifies repeated doubling
10. **Capacity at MaxCallDepth** — grow to exactly the cap, verify no overallocation

#### GC Under Deep Recursion (Critical)

11. **Recursive allocation stress** — deep recursion where each frame:
    - Allocates a closure (GC pressure)
    - Captures an upvalue (root scanning)
    - Creates a string (interning)
    - Triggers collection (stress GC mode)
12. **Verify MarkRoots traverses all frames** — deep stack with unique closures per frame, force GC, verify no unmarked frame closures survive

#### Exception / Unwind Integrity (Critical)

13. **Overflow → continue** — hit the limit, verify interpreter continues safely afterward (FrameCount restored, no half-initialized frame, no stale closure pointer)
14. **Overflow error message** — verify error includes function name and suggests `setMaxCallDepth()`
15. **Multiple overflows** — overflow, recover, overflow again — verify VM remains stable

#### Interaction Tests

16. **Deep recursion + value stack growth** — both stacks grow simultaneously; verifies `pushStack` rebase loop works with dynamic frame array
17. **Native call at depth** — verify native functions work correctly when frame stack has been reallocated

### Edge Cases

#### 1. `setMaxCallDepth(n)` where `n < VM.FrameCount`

If a script calls `setMaxCallDepth(10)` while already 50 frames deep, what should happen?

**Decision: Accept it as a soft limit (Python semantics).** The existing frames stay; the limit takes effect on the *next* call attempt. This matches `sys.setrecursionlimit()` in CPython — setting it low doesn't unwind the stack, it just means the next call beyond the limit fails. Simpler, less surprising, no need for error handling at the call site.

Validation: `newLimit >= 64` (practical floor), `newLimit <= 65536` (hard ceiling). Values below current `FrameCount` are accepted — they just mean the next call will fail.

#### 2. Local `frame` pointer between `ReadByteFr` and `CallValue`

In `OP_CALL`:
```pascal
argCount := ReadByteFr;           // reads via frame^ (the CALLER's ip)
if not CallValue(...) then ...    // may ReallocMem the frame array → old frame is dangling
frame := @VM.Frames[...];        // rebases immediately
```

`ReadByteFr` uses `frame^.ip` *before* `CallValue`. If `CallValue` grows the array, the old `frame` pointer becomes invalid. However, we **never dereference `frame` again** between `CallValue` returning and the rebase assignment. **Safe by ordering**, not by design.

**Mitigation:** Add a `// WARNING: frame pointer may be invalid after this call — do not use until rebased` comment above every `CallValue` / growth site. Any future code inserted between `CallValue` and the rebase would be a latent bug.

#### 3. Frame memory not tracked in `BytesAllocated`

The value stack already uses raw `ReallocMem` outside GC accounting. The frame array will follow the same pattern. This means `bytesAllocated()` underreports actual VM memory usage. Both stacks are "infrastructure" memory — consistent, but worth documenting.

No action needed — matches existing convention.

#### 4. `ReallocMem` failure (out of memory)

If `ReallocMem` fails, Delphi raises `EOutOfMemory`. This unwinds past `CallValue` and `Run`, leaving the VM in an inconsistent state (e.g., `FrameCount` may not match actual array). This is the **same existing risk** as the value stack — not new, not worse.

**Future mitigation (not this PR):** A try/except around `ReallocMem` that issues a runtime error instead of crashing. For now: accept the risk as consistent with existing behavior.

#### 5. GC cannot fire during `GrowFrameStack`

`GrowFrameStack` is only called from `CallValue`. GC is triggered by `Allocate` (which fires from `pushStack`, `newClosure`, etc.). `CallValue` does not allocate any GC-tracked objects before pushing the new frame. Therefore, GC cannot fire between "grow frame array" and "initialize new frame."

**Safe by code path analysis.** Add a comment: `// INVARIANT: No GC allocation between GrowFrameStack and frame initialization`.

#### 6. `OP_CLOSURE` loop reads many bytes from `frame^.ip`

```pascal
for i := 0 to closure^.upvalueCount - 1 do
begin
  isLocal := ReadByteFr;  // uses frame^
  index := ReadByteFr;    // uses frame^
  ...
end;
```

This loop uses `frame` heavily but never calls `CallValue` or triggers frame growth. **No risk.**

#### 7. Future re-entrancy (native → VM callback)

If native functions ever call back into the VM (re-entrant `Interpret`/`Run`), the outer `Run`'s `frame` pointer would go stale if the inner execution grows the frame array. The current design does not support re-entrancy.

**Mitigation:** Add a `// WARNING: GrowFrameStack invalidates all TCallFrame pointers. Re-entrant VM calls are not safe.` comment on `GrowFrameStack`. This is a landmine for future work, not a current risk.

### Risk Assessment

| Risk | Mitigation |
|---|---|
| Pointer invalidation after `ReallocMem` | `Run` always rebases `frame` after call/return — verified by audit |
| `frame` used between ReadByte and CallValue | Safe by ordering; add warning comment for future maintainers |
| Memory leak | `FreeVM` frees the array; assert `VM.Frames = nil` after |
| GC during growth | Frame array is raw memory; GC cannot fire in the growth window (no allocations) |
| `ReallocMem` failure | Same risk as value stack; accept for now |
| Performance regression | Indexed array access identical to before; initial 64 avoids most reallocations |
| Unbounded memory use | `MaxCallDepth` caps growth; hard ceiling at 65536 |
| Re-entrancy | Not supported; documented prominently on `GrowFrameStack` |
| `setMaxCallDepth` below current depth | Accepted (Python semantics); enforced on next call, not retroactively |
| Debugger/profiler caching frame pointers | WARNING comment on GrowFrameStack; documented invariant |
| TCO pressure (users recurse harder) | 65536 ceiling prevents memory exhaustion; TCO is a future project |

### Implementation Order

1. Add new constants (`FRAMES_INITIAL_CAPACITY`, `FRAMES_DEFAULT_MAX`, `FRAMES_HARD_MAX`, `FRAMES_GROWTH_FACTOR`), remove `FRAMES_MAX` / `STACK_MAX`
2. Add `TCallFrameArray` / `pCallFrameArray` types
3. Change `TVirtualMachine` record (Frames, FrameCapacity, MaxCallDepth)
4. Implement `GrowFrameStack` with capped growth + assertions + warning comments
5. Update `InitVM` (allocate initial array, set defaults) / `FreeVM` (free array)
6. Update `CallValue` (soft limit check → grow-or-error)
7. Update all `@VM.Frames[...]` → `@VM.Frames^[...]` access patterns
8. Add warning comments at every rebase site
9. Add native functions (`getMaxCallDepth`, `setMaxCallDepth`, `vmFrameCapacity`)
10. Write tests (all 17 categories above)
11. Run full test suite + manual smoke test
12. Review all assertions fire correctly under DEBUG_STRESS_GC

---

## Future Work (Not This PR)

### Tail-Call Optimization (TCO)

Once users can recurse to 1024+ frames, they **will** write:

```lox
fun loop(n) {
  if (n <= 0) return n;
  return loop(n - 1);
}
loop(1000000);
```

and expect it to work "forever." Without TCO, the 65536 ceiling is a hard wall. TCO reuses the current frame for tail-position calls, making such patterns O(1) in stack space.

This refactor (dynamic frames, separated policy) is a **prerequisite** for TCO — you need to understand frame lifecycle before you can optimize it away.

### `CurrentFrame` Inline Accessor

Replace all cached `frame` locals with an inline function that computes the pointer on demand. Eliminates the stale-pointer hazard class entirely. Deferred because it touches the hot loop and needs benchmarking.

### Coroutines / Fibers

Dynamic heap-allocated frame stacks are the natural foundation for:
- Coroutines (each gets its own frame stack)
- Green threads (scheduler swaps frame stack pointers)
- Generators (yield suspends a frame stack)

Static arrays make all of these painful. This refactor unblocks that future.
