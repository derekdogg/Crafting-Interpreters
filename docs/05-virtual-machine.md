# Virtual Machine

The VM is a stack-based bytecode interpreter with a call-frame stack for function calls.

## VM Structure

```pascal
TVirtualMachine = record
  Frames:          array[0..FRAMES_MAX-1] of TCallFrame;  // 64 frames max
  FrameCount:      integer;
  Stack:           pStack;            // operand stack
  MemTracker:      pMemTracker;       // GC + allocation tracking
  Strings:         pTable;            // string intern table
  Globals:         pTable;            // global variables
  OpenUpvalues:    pObjUpvalue;       // linked list of open upvalues
  RuntimeErrorStr: String;            // last runtime error
  PrintOutput:     String;            // accumulated print output
end;
```

### Call Frames

```pascal
TCallFrame = record
  closure: pObjClosure;   // the function being executed
  ip:      pByte;         // instruction pointer into chunk.Code
  slots:   pValue;        // base pointer into VM stack
end;
```

Each function call pushes a new frame. `slots` points to the callee's first stack slot (the function value itself), allowing local variable access as `frame^.slots[index]`.

### Operand Stack

```pascal
TStack = record
  Count:           integer;
  CurrentCapacity: integer;
  Values:          pValue;       // heap-allocated array
  StackTop:        pValue;       // next free slot
end;
```

The stack grows dynamically (2× when full) via raw `ReallocMem` to avoid triggering GC during push/pop.

## Dispatch Loop (`Run`)

The main execution function is a `while True` loop that reads and dispatches opcodes:

```pascal
function Run: TInterpretResult;
begin
  frame := @VM.Frames[VM.FrameCount - 1];
  while True do
  begin
    instruction := ReadByteFr;
    case instruction of
      OP_CONSTANT: ...
      OP_ADD: ...
      OP_RETURN: ...
      ...
    end;
  end;
end;
```

`ReadByteFr` reads one byte from `frame^.ip` and advances it. The frame-local functions (`ReadConstantFr`, `ReadConstantLongFr`) read constant operands.

## Instruction Set

### Constants & Literals

| Opcode | Operands | Effect |
|--------|----------|--------|
| `OP_CONSTANT` | 1-byte idx | Push `constants[idx]` |
| `OP_CONSTANT_LONG` | 3-byte idx | Push `constants[idx]` (24-bit) |
| `OP_NIL` | — | Push nil |
| `OP_TRUE` | — | Push true |
| `OP_FALSE` | — | Push false |

### Arithmetic & Logic

| Opcode | Effect |
|--------|--------|
| `OP_NEGATE` | Pop number, push negated |
| `OP_ADD` | Pop 2; number+number or string+string |
| `OP_SUBTRACT` | Pop 2 numbers, push difference |
| `OP_MULTIPLY` | Pop 2 numbers, push product |
| `OP_DIVIDE` | Pop 2 numbers, push quotient |
| `OP_MODULO` | Pop 2 numbers, push `A - Trunc(A/B)*B` |
| `OP_NOT` | Pop value, push boolean negation |
| `OP_EQUAL` | Pop 2, push boolean (deep equality) |
| `OP_GREATER` | Pop 2 numbers, push boolean |
| `OP_LESS` | Pop 2 numbers, push boolean |

### String Concatenation

`OP_ADD` checks if both operands are strings. If so, it calls `Concatenate` which:
1. Peeks both strings (keeping them on stack for GC safety)
2. Allocates the result via `AddString`
3. Pops both operands, pushes the result

### Variables

| Opcode | Operands | Effect |
|--------|----------|--------|
| `OP_DEFINE_GLOBAL` | 1-byte const idx | `globals[name] = pop()` |
| `OP_GET_GLOBAL` | 1-byte const idx | Push `globals[name]` |
| `OP_SET_GLOBAL` | 1-byte const idx | `globals[name] = peek()` |
| `OP_GET_LOCAL` | 1-byte slot | Push `frame.slots[slot]` |
| `OP_SET_LOCAL` | 1-byte slot | `frame.slots[slot] = peek()` |
| `OP_GET_UPVALUE` | 1-byte idx | Push via upvalue pointer |
| `OP_SET_UPVALUE` | 1-byte idx | Store via upvalue pointer |

Plus `_LONG` variants for globals (24-bit constant index).

### Control Flow

| Opcode | Operands | Effect |
|--------|----------|--------|
| `OP_JUMP` | 2-byte offset | Unconditional forward jump |
| `OP_JUMP_IF_FALSE` | 2-byte offset | Jump if top is falsey (no pop) |
| `OP_LOOP` | 2-byte offset | Unconditional backward jump |
| `OP_POP` | — | Discard top of stack |

Jump offsets are big-endian 16-bit unsigned values, giving a maximum jump of 65535 bytes.

### Function Calls

**`OP_CALL`** (1-byte argCount):
1. Peek the callee (below args on stack)
2. `CallValue` dispatches on type:
   - **Closure**: Verify arity, push new call frame
   - **Native**: Call Delphi function, pop args+callee, push result
   - **RecordType**: Verify field count, create record, copy args to fields
   - Otherwise: runtime error

**`OP_RETURN`**:
1. Pop return value
2. Close upvalues for the frame's stack window
3. Decrement `FrameCount`
4. If `FrameCount = 0` → exit with `INTERPRET_OK`
5. Otherwise: collapse stack to frame's base, push return value, restore parent frame

### Closures

**`OP_CLOSURE`** / **`OP_CLOSURE_LONG`**:
1. Read function constant
2. Create `TObjClosure` wrapping the function
3. Push closure onto stack
4. For each upvalue:
   - Read `isLocal` flag + index
   - If local: `captureUpvalue(frame.slots + index)`
   - If upvalue: copy from current closure's upvalue array

**`OP_CLOSE_UPVALUE`**:
1. Close upvalues pointing at `StackTop - 1`
2. Pop the value

### Upvalue Lifecycle

`captureUpvalue(local)`:
- Walk the open upvalue list (sorted by stack position, descending)
- If one already points to `local`, reuse it
- Otherwise, create a new one and insert it in sorted order

`closeUpvalues(last)`:
- Close all open upvalues pointing at or above `last`
- Copy value into `upvalue.closed`, redirect `location` to `&upvalue.closed`

### Records

**`OP_RECORD`** (1-byte fieldCount + N field-name indices + type-name index):
- Read field names from constant pool
- Allocate a persistent `fieldNames` array
- Create `TObjRecordType`, push onto stack

**`OP_GET_PROPERTY`** / **`OP_GET_PROPERTY_LONG`** (constant idx):
- Verify top of stack is a record
- Look up field by name → index
- Replace record on stack with field value

**`OP_SET_PROPERTY`** / **`OP_SET_PROPERTY_LONG`** (constant idx):
- Verify stack[-1] is a record
- Write stack top into the field
- Pop value and record, push value (assignment expression result)

### Native Object Method Invocation

**`OP_INVOKE`** / **`OP_INVOKE_LONG`** (name idx + argCount):
1. Peek receiver below args
2. Verify it's a native object
3. Look up method in `classInfo.methods`
4. Call the method function with (instance, argCount, args)
5. Pop args + receiver, push result

## Runtime Errors

Errors are signaled by:
```pascal
procedure RuntimeError(const msg: string);
begin
  VM.RuntimeErrorStr := msg;
end;
```

After calling `RuntimeError`, the dispatch loop checks and returns `INTERPRET_RUNTIME_ERROR`. The error string is propagated back through `TInterpretResult.ErrorStr`.

## Native Functions

Registered during `InitVM`:

| Name | Purpose |
|------|---------|
| `clock` | Wall-clock seconds (via `GetTickCount`) |
| `collectGarbage` | Force a GC cycle |
| `assert` | Runtime assertion (1-2 args) |
| `bytesAllocated` | Current `MemTracker.BytesAllocated` |
| `objectsAllocated` | Count of live objects |
| `newArray` | Create empty `TObjArray` |
| `arrayPush/Pop/Get/Set/Len/Remove` | Array operations |
| `StringList` | Create wrapped `TStringList` instance |

## IEEE 754 Semantics

```pascal
SetExceptionMask(exAllArithmeticExceptions);
```

This disables FPU exceptions, allowing `NaN`, `Inf`, and `-Inf` to propagate naturally through arithmetic rather than raising Delphi exceptions.
