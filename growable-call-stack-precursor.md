# Pre-Cursor: Strengthening Before Growable Frame Stack

These changes prepare the codebase for the dynamic frame array refactor.
Each is independently safe, testable, and makes the subsequent work less risky.

---

## 1. Decouple `CallValue` from `frame` (Critical)

### Problem

`CallValue` is a nested function inside `Run`. It captures `Run`'s `frame` local by reference and **writes to it** as a side effect:

```pascal
frame := @VM.Frames[VM.FrameCount];   // mutates Run's local
frame^.closure := ...
frame^.ip := ...
frame^.slots := ...
Inc(VM.FrameCount);
```

After `CallValue` returns, `Run` redundantly rebases:
```pascal
frame := @VM.Frames[VM.FrameCount - 1];
```

This means `frame` has **two owners** — `CallValue` sets it, `Run` resets it. With a static array this is harmless. With a dynamic array that moves on growth, the nested function would be writing through a pointer it just invalidated.

### Fix

Stop `CallValue` from touching `frame`. Write directly into the array by index:

```pascal
// Before (captures frame, mutates it):
frame := @VM.Frames[VM.FrameCount];
frame^.closure := pObjClosure(callee.ObjValue);
frame^.ip := pObjClosure(callee.ObjValue)^.func^.chunk^.Code;
frame^.slots := VM.Stack.StackTop;
Dec(frame^.slots, argCnt + 1);
Inc(VM.FrameCount);

// After (index-based, no frame mutation):
VM.Frames[VM.FrameCount].closure := pObjClosure(callee.ObjValue);
VM.Frames[VM.FrameCount].ip := pObjClosure(callee.ObjValue)^.func^.chunk^.Code;
VM.Frames[VM.FrameCount].slots := VM.Stack.StackTop;
Dec(VM.Frames[VM.FrameCount].slots, argCnt + 1);
Inc(VM.FrameCount);
```

After this change, `Run`'s rebase after `OP_CALL` is no longer redundant — it becomes the **single point of truth** for `frame`.

### Verification

- All existing tests pass (behavior unchanged)
- `frame` is only assigned in `Run` itself (at initialization, after OP_CALL, after OP_RETURN)
- `CallValue` no longer reads or writes `frame`

---

## 2. Remove dead `STACK_MAX` constant

### Problem

```pascal
STACK_MAX = FRAMES_MAX * UINT8_COUNT;  // = 65536
```

The value stack has been dynamic/growable for a while. `STACK_MAX` is never referenced anywhere else in the code. It's a leftover that suggests a fixed stack to anyone reading the constants.

### Fix

Delete the line. One less thing to confuse future maintainers.

### Verification

- Grep confirms no usage beyond the declaration

---

## 3. Add comment documenting `frame` ownership in `Run`

### Problem

The semantics of the `frame` local — when it's valid, who sets it, when it's stale — are implicit. Anyone adding code to the dispatch loop could accidentally use `frame` in a stale context.

### Fix

Add a block comment at the top of `Run`'s `while True` loop:

```pascal
// FRAME OWNERSHIP:
// `frame` always points to VM.Frames[VM.FrameCount - 1] (the current executing frame).
// It MUST be rebased after any operation that changes VM.FrameCount:
//   - After CallValue returns true (OP_CALL)
//   - After OP_RETURN decrements FrameCount
// Between ReadByteFr and CallValue, frame still points to the CALLER.
// After CallValue, frame is stale until explicitly rebased.
// No nested function may mutate frame.
```

Also annotate the local declaration itself:

```pascal
var
  frame : ^TCallFrame;  // Cached pointer to current executing frame.
                        // Derived from VM.Frames[VM.FrameCount - 1].
                        // Never authoritative state — recomputed after call/return.
```

### Verification

- No behavioral change; documentation only

---

## 4. Assert `frame` consistency at dispatch entry (DEBUG only)

### Problem

If `frame` ever becomes stale, bugs will be silent corruption. A cheap assertion at the top of the dispatch loop catches it immediately.

### Fix

Add at the start of the `while True` loop body:

```pascal
{$IFDEF DEBUG}
Assert(VM.FrameCount > 0,
  'Run dispatch: FrameCount <= 0');
Assert(frame = @VM.Frames[VM.FrameCount - 1],
  'Run dispatch: frame is out of sync with FrameCount');
{$ENDIF}
```

The `FrameCount > 0` guard catches underflow before it becomes a `[-1]` index. The sync check is the canary for any stale-pointer bug.

### Verification

- All tests pass with assertion enabled
- Zero perf cost in release builds

---

## 5. Document the `CallValue` nested-function contract

### Problem

`CallValue` accesses several of `Run`'s locals (`frame`, `native`, `nativeResult`, `value`, etc.) via closure capture. The contract for what it may read vs write is implicit.

### Fix

Add a comment above `CallValue`:

```pascal
// CallValue: Pushes a new call frame or executes a native/record call.
// CONTRACT:
//   - Does NOT read or write `frame` (Run rebases after return)
//   - May read `native`, `nativeResult`, `value` (shared locals)
//   - On success for closures: increments VM.FrameCount
//   - On success for natives/records: pops args, pushes result
//   - On failure: sets RuntimeErrorStr, returns false
```

### Verification

- Documentation only (but enforced by assertion #4 above)

---

## 6. Add `CurrentFrame` inline helper

### Problem

Every rebase site computes `@VM.Frames[VM.FrameCount - 1]` manually. When the frame array representation changes (static → dynamic, indexed → pointer arithmetic), every site must be updated individually.

### Fix

Centralize the access pattern:

```pascal
function CurrentFrame: pCallFrame; inline;
begin
  Result := @VM.Frames[VM.FrameCount - 1];
end;
```

Then transition rebase sites:

```pascal
// Before:
frame := @VM.Frames[VM.FrameCount - 1];

// After:
frame := CurrentFrame;
```

When the growable stack lands, only `CurrentFrame` changes — not every call site.

### Verification

- All tests pass (inline = zero overhead in optimized builds)
- `frame := CurrentFrame` at all existing rebase points
- Grep for `@VM.Frames[VM.FrameCount` should return zero hits outside `CurrentFrame` itself

---

## Summary

| # | Change | Risk | Effort |
|---|--------|------|--------|
| 1 | Decouple CallValue from frame | Low (behavioral no-op) | 10 min |
| 2 | Remove STACK_MAX | None | 1 min |
| 3 | Frame ownership comment + declaration annotation | None | 3 min |
| 4 | Frame consistency assertion (strengthened) | None (debug only) | 5 min |
| 5 | CallValue contract comment | None | 2 min |
| 6 | Add `CurrentFrame` inline helper | Low | 5 min |

Total: ~25 minutes of focused work, zero behavioral change, dramatically safer foundation for the growable frame refactor.

---

## Order of Operations

1. Item 2 (remove dead constant) — trivial, no deps
2. Item 1 (decouple CallValue) — the real prep work
3. Item 5 (document contract) — describes what we just enforced
4. Item 3 (frame ownership comment + declaration annotation) — context for the assertion
5. Item 6 (add CurrentFrame helper) — centralizes access semantics
6. Item 4 (add assertion) — validates everything above
7. Run full test suite
8. Commit: "prep: decouple CallValue from frame, add safety assertions"
