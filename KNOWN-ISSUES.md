# Known Issues & Future Work

Items we've identified but chosen not to address immediately.

## Call Frame Stack (fixed-size array)

**Status:** Bumped from 64 → 256. Still a static array.

The `Frames` array in `TVirtualMachine` is fixed at compile time. A dynamic/growable frame
stack (like the value stack) would remove the hard limit entirely. Challenges:
- Realloc would invalidate the `frame` pointer held in the run loop (same pattern as
  the stack rebase fix for upvalues)
- Would need index-based access or re-derive `frame` after any call that might grow

Production runtimes (Lua 200, CPython 1000, V8 ~15k) use either dynamic allocation or
the native OS stack. 256 is fine for now but may need revisiting if users write deep
recursion (tree traversals, interpreters-in-interpreters, etc.).

## Intern Table Tombstone Bloat Under DEBUG_STRESS_GC

**Status:** Observed via `internTableStats()`, no fix applied.

Under `DEBUG_STRESS_GC`, every allocation triggers GC → `TableRemoveWhite` tombstones
any dead intermediate strings → tombstones accumulate 1:1 with temporary string
allocations. `AdjustCapacity` correctly discards tombstones on resize, but new ones are
immediately created during the resize itself (allocations trigger more GCs).

This is a debug-mode-only artifact. In normal builds, GC fires infrequently and
tombstones stay low between resizes. Possible mitigations:
- Skip `TableRemoveWhite` if tombstone ratio is already high (defer to next resize)
- Track live vs tombstone counts separately in `TTable` (like `TObjDictionary` does)
- Resize in-place without allocating (impossible with current `Allocate` API)

## TTable.Count Semantics

**Status:** Working as designed (book's pattern), but confusing for users.

`TTable.Count` = live entries + tombstones. Used for load-factor calculation (correct),
but means `Count` doesn't answer "how many strings are interned?" without subtracting
tombstones. The `internTableStats()` native works around this by counting separately.

If `TTable` is ever refactored, consider splitting into `LiveCount` + `TombstoneCount`
to match `TObjDictionary`'s clearer design.
