# Event System Architecture

This document explains how the callback/event infrastructure works in the Lox interpreter, tracing the flow from script code through compilation, VM execution, event registration, and finally callback dispatch.

---

## Design Philosophy

The event system requires **no language extensions**. There are no special keywords, no custom opcodes, and no syntax changes. Events are implemented entirely through:

- **Native functions** for registration (`onKeyPressed`, `onClick`, etc.)
- **First-class closures** as callback values
- **A dedicated event engine** (`TLoxEventEngine`) that queues and dispatches events
- **Re-entrant VM execution** via `InvokeCallback` to fire callbacks mid-script

This means the compiler sees event code as ordinary function calls with lambda arguments — the entire event mechanism lives in the runtime layer.

---

## The Golden Rule

> **Events are never dispatched automatically.**
>
> The script **must** call `processEvents()` regularly. If `processEvents()` is never called, registered callbacks will never execute — no matter how many events the OS has queued.

This is the single most important thing to understand about the system. Registration only stores a closure; dispatch only happens when the script explicitly asks for it.

---

## Script Perspective

From a Lox script, events look like this:

```lox
// Register callbacks (canvas-level, no target control)
onKeyPressed(fun(key) {
  print "Pressed: " + key;
});

onMouseDown(fun(btn, x, y) {
  print "Click at " + str(x) + "," + str(y);
});

// Register callbacks targeting a specific named control
onClick("button1", fun() {
  print "Button 1 clicked!";
});

onMouseDown("panel1", fun(btn, x, y) {
  print "Panel click";
});

// Event loop — script must explicitly pump events
var running = true;
onKeyPressed(fun(key) {
  if (key == "escape") running = false;
});

while (running) {
  processEvents();
}
```

Key points:
- Registration functions accept either 1 argument (closure only → canvas default) or 2 arguments (control name + closure)
- `processEvents()` is the heartbeat — it pumps the Windows message queue and dispatches any pending events
- Without `processEvents()` in a loop, no events will fire

### Available Event Functions

| Function | Callback Signature | Notes |
|----------|-------------------|-------|
| `onKeyPressed` | `fun(key)` | Fires once on initial key-down (no auto-repeat) |
| `onKeyReleased` | `fun(key)` | Fires on key-up |
| `onKeyHeld` | `fun(key)` | Fires every frame for each currently-held key |
| `onMouseDown` | `fun(btn, x, y)` | btn: 0=left, 1=right, 2=middle |
| `onMouseUp` | `fun(btn, x, y)` | Same as above |
| `onMouseMove` | `fun(x, y)` | Coalesced: only latest position per frame |
| `onClick` | `fun()` | Named-control clicks only (requires control name) |
| `processEvents` | (no callback) | Pumps messages and dispatches queued events |

### Callback Replacement & Removal

Registering a callback for the same event+control **replaces** the previous one. There is no append/multi-handler behaviour:

```lox
onKeyPressed(fun(k) { print "A"; });
onKeyPressed(fun(k) { print "B"; });
// Only "B" fires — the first callback is gone.
```

There is **no explicit unregister mechanism** (no `offKeyPressed` or similar). Callbacks persist for the lifetime of the script. To effectively disable a callback, replace it with a no-op:

```lox
onKeyPressed(fun(k) {}); // Replaces previous handler with an empty one
```

All callbacks are cleared automatically when the script terminates (via `Engine.Reset`).

---

## Compilation: No Special Treatment

The compiler handles event registrations identically to any other function call. For example:

```lox
onKeyPressed(fun(key) { print key; })
```

Generates the following bytecode:

```
OP_CONSTANT <lambda_index>      // Push the closure (compiled function)
OP_GET_GLOBAL "onKeyPressed"    // Look up the native function
OP_CALL 1                       // Call with 1 argument
OP_POP                          // Discard nil return value
```

For a named-target variant like `onClick("button1", fun() { ... })`:

```
OP_CONSTANT "button1"           // Push control name string
OP_CONSTANT <lambda_index>      // Push the closure
OP_GET_GLOBAL "onClick"         // Look up native
OP_CALL 2                       // Call with 2 arguments
OP_POP
```

The lambda itself compiles as a standard `OP_CLOSURE`, with its body containing normal bytecode. There is nothing event-specific in the compilation stage.

---

## VM Execution: Native Registration

When the VM executes `OP_CALL` on a native function like `onKeyPressed`, control passes to the native implementation in `EventNatives.pas`.

### Argument Parsing

All event registration natives use a common pattern to handle both the 1-arg and 2-arg forms:

```
Arguments:
  1-arg form:  args[0] = closure                    → controlName = '' (canvas default)
  2-arg form:  args[0] = string, args[1] = closure  → controlName = args[0]
```

The native validates argument types, extracts the control name and closure, then delegates to the event engine:

```
Engine.SetCallback(eckKeyPressed, controlName, closure)
```

The native returns `nil` to the script.

---

## Event Engine: Registration & Storage

`TLoxEventEngine` (in `LoxEventEngine.pas`) is the central coordinator. It maintains:

### Callback Registry

A dictionary (`FCallbackSlots`) keyed by `"<EventKindOrd>:<ControlName>"`:

```
Key examples:
  "0:"         → onKeyPressed for canvas (no control)
  "0:panel1"   → onKeyPressed for "panel1"
  "6:button1"  → onClick for "button1"
```

Each entry holds a `TCallbackSlot` object containing the `TValue` closure. The slot is heap-allocated so its address remains stable — this matters for GC root registration (see below).

### Event Queues

Pending events are stored in string lists until dispatched:

| Queue | Format | Example |
|-------|--------|---------|
| `FPendingKeys` | `"down:sender:keyname"` / `"up:sender:keyname"` | `"down::escape"` |
| `FPendingMouse` | `"down:sender:btn:x:y"` / `"up:sender:btn:x:y"` | `"down:panel1:0:120:45"` |
| `FPendingClicks` | `"controlname"` | `"button1"` |
| `FMouseMoveX/Y` | Coordinates (coalesced) | Latest position only |
| `FHeldKeys` | Key names currently held | `["a", "shift"]` |

---

## Host Integration: VCL → Event Engine

The Delphi form (`fmEventTest.pas`) wires standard VCL events to the engine's queue methods:

```
FormKeyDown(Key)       →  Engine.QueueKeyDown(MapVKToName(Key))
FormKeyUp(Key)         →  Engine.QueueKeyUp(MapVKToName(Key))
Panel1MouseDown(...)   →  Engine.QueueMouseDown(btn, x, y, 'panel1')
Button1Click(...)      →  Engine.QueueClick('button1')
```

Events are **queued, not dispatched immediately**. The queue is only drained when the script calls `processEvents()`. This ensures callbacks execute at a predictable point in the script's control flow, not at an arbitrary interrupt.

---

## Event Dispatch: processEvents()

When the script calls `processEvents()`, the native function does three things in order:

1. **Flush output** — ensures any buffered print output reaches the display
2. **Pump the message queue** — calls `Application.ProcessMessages`, which may trigger VCL handlers that queue more events
3. **Dispatch pending events** — iterates all queues and fires registered callbacks

### Dispatch Order

Events are processed in this fixed category order:

1. Key-pressed events (from `FPendingKeys` where type = "down")
2. Key-released events (from `FPendingKeys` where type = "up")
3. Mouse-down events (from `FPendingMouse` where type = "down")
4. Mouse-up events (from `FPendingMouse` where type = "up")
5. Click events (from `FPendingClicks`)
6. Mouse-move (coalesced — single dispatch with latest coordinates)
7. Held-key callbacks (fires once per held key, every frame)

**Within each category, events are dispatched in FIFO order** (insertion order via `TStringList`). If the user presses A then B then C, the callbacks fire in that sequence.

After all categories are dispatched, queues are cleared.

### Error Handling

If a callback raises a runtime error (e.g., referencing an undefined variable), dispatch **halts immediately**:

- `VM.RuntimeErrorStr` is set to the error message
- The dispatch loop checks this flag after every `InvokeCallback` call and exits on error
- **All remaining queued events are abandoned** — they are not dispatched
- Control returns to the script, which will see the runtime error

```lox
onKeyPressed(fun(key) {
  undefinedVariable;  // Runtime error here
});

onKeyReleased(fun(key) {
  print "released";   // Never fires for this frame's events
});
```

---

## Callback Invocation: InvokeCallback

The critical function that bridges the event engine back into the VM is `InvokeCallback` (in `Suto.pas`). It executes a Lox closure from Delphi code:

### Execution Steps

1. **Validate** — confirms the value is actually a closure
2. **Arity check** — argument count must match the closure's parameter count
3. **Stack overflow guard** — ensures frame count is within limits
4. **Push onto VM stack** — pushes the closure (slot 0) followed by arguments (slots 1..N)
5. **Create call frame** — sets up a new `CallFrame` pointing at the closure's bytecode
6. **Execute** — calls `Run(frameIndex)` which runs bytecode until `OP_RETURN`
7. **Balance stack** — pops the return value and asserts stack/frame integrity

In debug builds (`DEBUG_ASSERT_INVARIANTS`), two assertions fire after successful return:

```
Assert((StackTop - Values) == savedStackDepth)   // No stack leak or underflow
Assert(FrameCount == savedFrameCount)            // No orphaned frames
```

These use byte-offset arithmetic (not raw pointers) so they remain valid even if the stack is reallocated during execution.

### Re-Entrancy

`InvokeCallback` is fully re-entrant. A callback can:
- Call other Lox functions
- Trigger further native calls
- Even call `processEvents()` again (nested dispatch)

The VM handles this naturally because each invocation pushes a new frame onto the existing call stack.

---

## Re-Entrancy: Rules and Caveats

The event system **permits** nested callback execution, but this creates important constraints that maintainers must understand.

### What is safe during a callback

- **Replacing a callback** (`onKeyPressed(fun(k) { ... })`) — modifies `FCallbackSlots` but does not affect the current dispatch iteration, which iterates over the event queues, not the slot dictionary. The new callback takes effect on the next `processEvents()` call.
- **Calling normal Lox functions** — standard re-entrant frame push.
- **Reading/writing script variables** — closures capture upvalues normally.

### What is dangerous

**Nested `processEvents()` inside a callback:**

```lox
onKeyPressed(fun(key) {
  processEvents();  // ⚠️ Nested dispatch
});
```

This is technically legal but has consequences:

1. The nested `DispatchPendingEvents()` iterates the **same queues** as the outer call
2. The nested call **clears the queues** when it finishes
3. The outer iteration continues but the queue is already empty
4. Events queued *during* the nested dispatch may be silently lost

There are **no guards against infinite recursion** beyond the VM's frame limit (`FRAMES_MAX = 512`). A callback that unconditionally calls `processEvents()` while events keep arriving will eventually stack overflow.

### The held-key exception

Held-key dispatch (`onKeyHeld`) takes a **snapshot** of `FHeldKeys` before iterating, making it safe against mutation during dispatch. Other queues do not have this protection.

### Practical guidance

- **Do not call `processEvents()` inside a callback** unless you have a specific reason and understand the implications
- **Do not rely on events queued during dispatch** being processed in the same frame
- **Self-replacing callbacks are safe** — the new closure takes effect next frame

---

## Parameter Passing

Arguments are marshalled from Delphi types into `TValue` (NaN-boxed UInt64) before being pushed onto the VM stack:

```
Key name "a"    →  CreateStringValue("a")    →  OBJ_TAG | pointer to ObjString
Mouse btn 0     →  CreateNumber(0)           →  IEEE 754 double bits
Coordinates     →  CreateNumber(x)           →  IEEE 754 double bits
```

Inside the callback, these appear as regular local variables accessed via `OP_GET_LOCAL`.

---

## Garbage Collection Integration

Closures stored in the event engine live outside the VM's normal reachability graph (they aren't on the stack or in globals while the script runs between `processEvents()` calls). Without special handling, the GC would collect them.

### Solution: GC Roots

When a callback is registered, its `TCallbackSlot` address is added to `VM.ExtraRoots[]`. The garbage collector's mark phase walks this array in addition to the stack and globals, ensuring registered callbacks and their upvalues remain alive.

```
Closure (TValue)
   │
   ▼
TCallbackSlot.Callback    ← stable heap address
   │
   ▼
VM.ExtraRoots[]           ← stores @slot.Callback
   │
   ▼
GC Mark Phase             ← marks the closure + its upvalues as reachable
```

### Lifecycle

- **First registration**: `SetCallback` creates a new `TCallbackSlot`, stores the closure, and calls `RegisterGCRoot(@slot.Callback)`
- **Replacement**: `SetCallback` overwrites `slot.Callback` in-place. The GC root points to the *slot address* (not the closure value), so the root automatically tracks the new closure. The old closure becomes unreferenced and eligible for collection. No root cleanup is needed on replacement.
- **Script termination**: `UnregisterGCRoots` removes all slots from `VM.ExtraRoots`
- **Stability**: Slots are heap-allocated class instances, so their address never changes even if the dictionary rehashes

### Why this matters

If a callback's closure (and its captured upvalues) were collected while still registered, invoking it would crash the VM with a use-after-free. The GC root mechanism is the safety net that prevents this.

---

## Complete Flow: End to End

Here's the full lifecycle of an event, from registration to callback execution:

```
┌─────────────────────────────────────────────────────────────────────┐
│ 1. SCRIPT                                                           │
│    onKeyPressed(fun(key) { print key; })                            │
│                                                                     │
│ 2. COMPILER                                                         │
│    Emits: OP_CONSTANT(lambda), OP_GET_GLOBAL, OP_CALL(1), OP_POP   │
│                                                                     │
│ 3. VM executes OP_CALL → native onKeyPressedNative()                │
│                                                                     │
│ 4. NATIVE FUNCTION                                                  │
│    Validates closure, calls Engine.SetCallback(eckKeyPressed, '', λ)│
│                                                                     │
│ 5. EVENT ENGINE                                                     │
│    Stores closure in FCallbackSlots["0:"], registers GC root        │
│                                                                     │
│ 6. SCRIPT continues... enters event loop                            │
│    while (running) { processEvents(); }                             │
└─────────────────────────────────────────────────────────────────────┘

         ... time passes, user presses 'A' ...

┌─────────────────────────────────────────────────────────────────────┐
│ 7. WINDOWS                                                          │
│    WM_KEYDOWN message arrives                                       │
│                                                                     │
│ 8. VCL HANDLER (FormKeyDown)                                        │
│    Calls Engine.QueueKeyDown("a")                                   │
│    → FPendingKeys.Add("down::a")                                    │
│                                                                     │
│ 9. SCRIPT calls processEvents()                                     │
│    → processEventsNative()                                          │
│    → Application.ProcessMessages (may queue more events)            │
│    → Engine.DispatchPendingEvents()                                 │
│                                                                     │
│ 10. DISPATCH                                                        │
│     Finds "down::a" in FPendingKeys                                 │
│     Looks up callback: FCallbackSlots["0:"] → closure               │
│     Marshals args: [CreateStringValue("a")]                         │
│     Calls InvokeCallback(closure, args, result)                     │
│                                                                     │
│ 11. VM RE-ENTRY                                                     │
│     Pushes closure + "a" onto stack                                 │
│     Creates new CallFrame                                           │
│     Run() executes: OP_GET_LOCAL(0) → OP_PRINT → OP_RETURN         │
│                                                                     │
│ 12. RETURN                                                          │
│     InvokeCallback returns, stack balanced                          │
│     processEvents() returns nil to script                           │
│     while loop iterates again                                       │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| No custom syntax or opcodes | Keeps the language simple; events are a library concern, not a grammar concern |
| Queued dispatch (not immediate) | Callbacks fire at a deterministic point in the script, avoiding re-entrancy surprises |
| Explicit `processEvents()` pump | Script controls its own frame rate and event granularity; no hidden magic |
| Replace semantics (not append) | One handler per event+control keeps dispatch predictable; no ordering ambiguity |
| No unregister API | Simplicity — replace with no-op to disable; full cleanup on script termination |
| Coalesced mouse-move | Prevents event flooding; only the latest position matters per frame |
| Held-key as separate event | Games need per-frame "is key down" without relying on OS auto-repeat timing |
| Held-key snapshot | Protects iteration against mutation during dispatch |
| GC root slots | Closures live in the engine, outside normal VM reachability — must be explicitly rooted |
| Error halts dispatch | Fail-fast on runtime errors; don't silently skip broken callbacks |
| Named-target overloads | Same API pattern for canvas events and per-control events; only differs by first arg |

---

## Testing Without VCL

The system includes simulation natives for scripted testing without a GUI:

```lox
simulateKeyDown("a");
simulateKeyUp("a");
simulateMouseDown(0, 100, 200);
processEvents();  // Dispatches simulated events as if they were real
```

These bypass the VCL handler layer and inject directly into the engine's queues, allowing automated test scripts to verify callback behavior.
