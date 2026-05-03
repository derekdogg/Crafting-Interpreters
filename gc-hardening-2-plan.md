# GC Hardening Phase 2 — Assertion Plan

## Invariants

1. **All roots must be valid** — Every value on the VM stack, in call frames, and in open upvalues must have a valid ValueKind/ObjectKind.
2. **All references must be valid** — Every child pointer followed during BlackenObject must point to a valid object (or be nil).
3. **Never free a live (marked) object** — Sweep must only free unmarked objects.
4. **Never use a freed object** — Poison freed objects so any subsequent access is detectable.
5. **All reachable objects must be marked** — After marking, every object reachable from roots must have IsMarked = true (DEBUG_STRESS_GC only).
6. **All allocations must be tracked** — Every allocated object must appear in CreatedObjects (DEBUG_STRESS_GC only).

Master invariant: Every object pointer anywhere must either be nil or point to a valid, tracked object.

## Implementation Steps

- [ ] 1. Add `AssertValidObjPointer` helper
- [ ] 2. Add ObjectKind validity assertion in `MarkObject`
- [ ] 3. Add "never free marked" assertion in `Sweep`
- [ ] 4. Add poison pattern in `FreeObject` (DEBUG_STRESS_GC)
- [ ] 5. Add root validity assertions in `MarkRoots`
- [ ] 6. Add child reference assertions in `BlackenObject`
- [ ] 7. Add post-mark reachability verification (DEBUG_STRESS_GC)
- [ ] 8. Add tracked-allocation verification in allocators (DEBUG_STRESS_GC)
