# Proposal: `OP_SELF_CALL` — Eliminate Global Lookups for Recursive Calls

## Problem

In `fib(40)`, the opcode pair profiler shows:

```
OP_GET_GLOBAL -> OP_GET_LOCAL_CONST_SUBTRACT : 331,160,280
```

Every recursive call compiles as:

```
OP_GET_GLOBAL <name_idx>     ; hash-table lookup of "fib" in vm.Globals
OP_GET_LOCAL_CONST_SUBTRACT  ; compute arg (n-1 or n-2)
OP_CALL 1                    ; invoke
```

The `OP_GET_GLOBAL` performs a `TableGet` against the global hash table 331M times per call site (662M total for both `fib(n-1)` and `fib(n-2)`). The result is always the same closure that's already sitting in `frame^.slots[0]` (slot 0 = the function itself, as per clox convention).

## Proposed Solution

Add **`OP_SELF_CALL`** — a specialized call opcode that reuses the current frame's closure instead of looking up a global.

### Semantics

```
OP_SELF_CALL <argCount>
```

Equivalent to:

```
push frame^.closure   (the callee — already known)
<arguments are already on stack>
OP_CALL <argCount>
```

But without:
- The `OP_GET_GLOBAL` dispatch + hash lookup
- Pushing the callee onto the stack (we write it directly into the new frame's slot 0)

### Bytecode Encoding

| Byte | Meaning |
|------|---------|
| `OP_SELF_CALL` | opcode |
| arg count | 1 byte (always matches current function's arity for true self-recursion, but we don't require it) |

The compiler pushes args normally, then emits `OP_SELF_CALL` instead of `OP_GET_GLOBAL` + args + `OP_CALL`.

Wait — there's a subtlety. The current clox/lox calling convention expects the callee in `slots[0]`. Today the sequence is:

```
GET_GLOBAL fib   → pushes closure onto stack (becomes slot 0 of new frame)
arg1             → slot 1
CALL 1
```

For `OP_SELF_CALL`, we need the closure in slot 0 of the *new* frame. Two options:

**Option A — Compiler inserts a slot-0 placeholder:**

The compiler emits `OP_GET_LOCAL_0` (or a new `OP_PUSH_SELF`) before the arguments, then `OP_SELF_CALL`. This keeps the stack layout identical. The win is replacing a hash-table lookup with a direct slot read (one pointer deref).

**Option B — VM writes slot 0 itself:**

The compiler emits args directly (no callee push). The VM handler for `OP_SELF_CALL` writes `frame^.closure` into the slot below the arguments itself. This saves one push + one dispatch but requires the VM to shift the stack conceptually.

### Recommendation: Option A (simplest, safest)

Option A changes minimal code:
- **Compiler**: detect self-calls, emit `OP_GET_LOCAL_0` + args + `OP_SELF_CALL` instead of `OP_GET_GLOBAL` + args + `OP_CALL`.
- **VM**: `OP_SELF_CALL` handler is identical to `OP_CALL`'s closure fast-path, except it skips the `isClosure(value)` type check (we *know* it's the current closure).

Actually, even simpler: since slot 0 already holds the function's own closure, `OP_GET_LOCAL_0` pushes the right value. So the only change is **in the compiler** — recognizing the self-call pattern and potentially skipping the global lookup by emitting `OP_GET_LOCAL_0` instead of `OP_GET_GLOBAL <idx>`.

Wait — this *already works* without a new opcode if `fib` were a local. The issue is that `fib` is declared at the top level via `funDeclaration`, which calls `defineVariable(global)` — storing it as a *global*, so `namedVariable` resolves it via `identifierConstant` → `OP_GET_GLOBAL`.

### Revised Simpler Approach: Compiler Self-Call Detection

When compiling a function call where:
1. We are inside a `TYPE_FUNCTION` compiler, AND
2. The callee name matches `Current^.func^.name`, AND
3. The name resolves as a global (not shadowed by a local/upvalue)

Instead of emitting `OP_GET_GLOBAL <idx>`, emit `OP_GET_LOCAL_0` (the slot-0 self-reference).

**This requires NO new opcode.** The existing `OP_CALL` works unchanged because the stack layout is identical.

The win: replace a `TableGet` hash lookup (331M×2 = 662M times) with a single array dereference that's already in the L1 cache.

---

## Implementation Plan

### Step 1 — Compiler change in `namedVariable`

After `resolveLocal` returns -1 and `resolveUpvalue` returns -1 (i.e., it's a global), add a check:

```pascal
// Self-call optimization: if we're reading a global whose name matches
// the current function's name, use slot 0 instead of a hash lookup.
if (Current^.funcType = TYPE_FUNCTION) and
   (Current^.func^.name <> nil) and
   identifiersEqual(name, Current^.func^.name) and
   (not canAssign or not check(TOKEN_EQUAL)) then  // not an assignment target
begin
  emitByte(OP_GET_LOCAL_0, CurrentChunk, parser.previous.line, vm.MemTracker);
  Exit;
end;
```

Where `identifiersEqual` compares the token text against the function's name string.

### Step 2 — Helper function

```pascal
function tokenMatchesFuncName(const tok: TToken; funcName: pObjString): Boolean;
begin
  Result := (funcName <> nil) and
            (tok.length = funcName^.length) and
            CompareMem(tok.start, @funcName^.chars[0], tok.length);
end;
```

### Step 3 — Testing

- `fib(40)` must produce the same result (102334155)
- Profile should show `OP_GET_GLOBAL` count drops by ~662M
- `OP_GET_LOCAL_0 -> OP_GET_LOCAL_CONST_SUBTRACT` becomes the hot pair instead
- All existing tests must pass (function reassignment edge cases below)

### Step 4 — Verify correctness for reassignment

Edge case:

```lox
fun fib(n) {
  if (n < 2) return n;
  return fib(n-1) + fib(n-2);
}
fib = someOtherFunction;  // reassign the global
fib(10);                  // should call someOtherFunction
```

With our optimization, the *compiled* `fib` body uses slot 0 (its own closure), so even if the global `fib` is reassigned, recursive calls within the original `fib` still call the original. This is actually **desirable and correct** — it matches how closures work in most languages (the function refers to itself, not to whatever the global variable happens to hold later).

However, mutual recursion patterns and forward references won't benefit from this optimization (they'd still use `OP_GET_GLOBAL`), which is fine.

If this semantic change is unacceptable (i.e., you want `fib` reassignment to affect in-progress recursions), we can gate the optimization behind a flag or skip it. But practically, no sane program reassigns a function mid-recursion.

---

## Expected Performance Impact

| Metric | Before | After (estimated) |
|--------|--------|-------------------|
| `OP_GET_GLOBAL` dispatches in fib(40) | ~662M | ~0 (for self-calls) |
| Hash table lookups eliminated | 662M | — |
| New `OP_GET_LOCAL_0` dispatches | 0 | +662M (but ~3 instructions each vs. ~20 for hash lookup) |
| Estimated wall-clock improvement | — | 5–15% on fib(40) |

The actual improvement depends on how expensive `TableGet` is relative to a slot-0 read. Given NaN boxing means `TValue` is 8 bytes and slot 0 is at a fixed offset from `frame^.slots`, this should be a single `MOV` instruction on x64.

---

## Alternatives Considered

### Full `OP_SELF_CALL` opcode (fused GET_LOCAL_0 + CALL)

Saves one extra dispatch per recursive call. Could be a follow-up if the simple approach shows remaining dispatch overhead at the `OP_GET_LOCAL_0 -> OP_CALL` transition.

Encoding: `OP_SELF_CALL <argCount>` — handler does everything `OP_CALL` does but reads closure from slot 0 directly. Eliminates one dispatch and the `isClosure` type check.

### `OP_GET_LOCAL_CONST_SUBTRACT_SELF_CALL` (mega-fusion)

Fuses argument computation and call into one opcode. Extremely specialized to fib-like patterns. Probably not worth the opcode-table space for general use.

### Tail-call optimization

Not applicable to fib (not tail-recursive). Would help `fib` rewritten in CPS style, but that's a different program.

---

## Risks

1. **Semantic change for global reassignment** — Discussed above. Considered acceptable.
2. **Name comparison cost at compile time** — Negligible (once per call site, not per execution).
3. **Interaction with closures/upvalues** — None. If the function captures itself via upvalue (nested function), `resolveUpvalue` would find it first and this path wouldn't trigger.
4. **Methods** — `TYPE_FUNCTION` check excludes methods (which would be `TYPE_METHOD` if you have that). Safe.

---

## Summary

The simplest, highest-value change is a **5-line compiler modification** in `namedVariable` that emits `OP_GET_LOCAL_0` when the callee is the current function. No new opcodes, no VM changes, no new test infrastructure — just eliminating 662M hash lookups.
