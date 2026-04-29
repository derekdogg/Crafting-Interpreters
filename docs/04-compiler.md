# Compiler (Parser + Bytecode Emitter)

The compiler is a single-pass Pratt parser that directly emits bytecode. There is no AST — tokens are consumed and opcodes are written to the current chunk in one pass.

## Compiler State

```pascal
TCompiler = record
  enclosing:  pCompiler;                      // parent (for nested functions)
  func:       pObjFunction;                   // function being compiled
  funcType:   TFunctionType;                  // TYPE_FUNCTION or TYPE_SCRIPT
  locals:     array[0..255] of TLocal;        // local variable stack
  localCount: integer;
  upvalues:   array[0..255] of TUpvalue;      // captured variables
  scopeDepth: integer;                        // nesting depth (0 = global)
end;
```

Compilers form a linked list via `enclosing`. When compiling a nested function, a new compiler is pushed; when done, it's popped and the parent resumes.

### Local Variables

```pascal
TLocal = record
  name:       TToken;      // source token (pointer into source)
  depth:      integer;     // scope depth (-1 = uninitialized sentinel)
  isCaptured: boolean;     // true if captured by a closure
end;
```

Locals are indexed by position (slot number on the stack). The `-1` depth sentinel prevents use-before-initialization (`var x = x;`).

### Upvalues

```pascal
TUpvalue = record
  index:   byte;       // slot index in enclosing function
  isLocal: boolean;    // true = captures a local; false = captures an upvalue
end;
```

## Parser State

```pascal
TParser = record
  Current:   TToken;     // lookahead token
  Previous:  TToken;     // just-consumed token
  HadError:  boolean;
  PanicMode: boolean;    // suppress cascading errors
  ErrorStr:  String;     // accumulated error messages
end;
```

## Pratt Parsing

The core of expression parsing is `parsePrecedence`:

```pascal
procedure parsePrecedence(precedence: TPrecedence);
```

1. Advance and look up the **prefix** rule for `parser.previous.tokenType`
2. If no prefix rule → error ("Expect expression.")
3. Call the prefix handler (e.g., `number`, `unary`, `grouping`, `variable`)
4. While the current token's precedence is ≥ the caller's level:
   - Advance and call the **infix** handler (e.g., `binary`, `call`, `dot_`, `and_`, `or_`)
5. Check for stray `=` (invalid assignment target)

### Parse Rules Table

```pascal
const Rules: array[TTokenType] of TParseRule = (
  { TOKEN_LEFT_PAREN }  (Prefix: grouping; Infix: call;   Precedence: PREC_CALL),
  { TOKEN_DOT }         (Prefix: nil;      Infix: dot_;   Precedence: PREC_CALL),
  { TOKEN_MINUS }       (Prefix: unary;    Infix: binary; Precedence: PREC_TERM),
  { TOKEN_PLUS }        (Prefix: nil;      Infix: binary; Precedence: PREC_TERM),
  { TOKEN_STAR }        (Prefix: nil;      Infix: binary; Precedence: PREC_FACTOR),
  { TOKEN_IDENTIFIER }  (Prefix: variable; Infix: nil;    Precedence: PREC_NONE),
  { TOKEN_NUMBER }      (Prefix: number;   Infix: nil;    Precedence: PREC_NONE),
  { TOKEN_STRING }      (Prefix: ParseString; Infix: nil; Precedence: PREC_NONE),
  { TOKEN_AND }         (Prefix: nil;      Infix: and_;   Precedence: PREC_AND),
  { TOKEN_OR }          (Prefix: nil;      Infix: or_;    Precedence: PREC_OR),
  ...
);
```

### Precedence Levels

```
PREC_NONE < PREC_ASSIGNMENT < PREC_OR < PREC_AND < PREC_EQUALITY
  < PREC_COMPARISON < PREC_TERM < PREC_FACTOR < PREC_UNARY < PREC_CALL < PREC_PRIMARY
```

## Bytecode Emission

All bytecode is written through:

```pascal
procedure emitByte(value: byte; Chunk: pChunk; Line: integer; MemTracker: pMemTracker);
```

Helper functions:
- `emitReturn` — emits `OP_NIL` + `OP_RETURN`
- `emitConstant(value)` — adds value to constant pool, emits `OP_CONSTANT` or `OP_CONSTANT_LONG`
- `emitJump(instruction)` — emits jump with 2-byte placeholder, returns patch offset
- `patchJump(offset)` — back-patches the jump distance
- `emitLoop(loopStart)` — emits `OP_LOOP` with backward offset

### Long Operands (24-bit)

When the constant pool exceeds 255 entries, the compiler switches to `_LONG` variants:

```pascal
if idx <= High(Byte) then
begin
  writeChunk(Chunk, OP_CONSTANT, Line, MemTracker);
  WriteChunk(Chunk, idx, Line, MemTracker);
end
else
begin
  writeChunk(Chunk, OP_CONSTANT_LONG, Line, MemTracker);
  IntBytes := IntToBytes(idx);
  WriteChunk(Chunk, IntBytes.byte0, Line, MemTracker);
  WriteChunk(Chunk, IntBytes.byte1, Line, MemTracker);
  WriteChunk(Chunk, IntBytes.byte2, Line, MemTracker);
end;
```

The 24-bit index supports up to 16 million constants. Affected opcodes: `OP_CONSTANT_LONG`, `OP_DEFINE_GLOBAL_LONG`, `OP_GET_GLOBAL_LONG`, `OP_SET_GLOBAL_LONG`, `OP_CLOSURE_LONG`, `OP_GET_PROPERTY_LONG`, `OP_SET_PROPERTY_LONG`, `OP_INVOKE_LONG`.

## Variable Resolution

### Globals

```pascal
function identifierConstant(const name: TToken): integer;
```

Stores the variable name as a string constant. At runtime, the VM uses `TableGet`/`TableSet` on the globals hash table with this string as the key.

### Locals

```pascal
function resolveLocal(compiler: pCompiler; const name: TToken): integer;
```

Walks the `locals` array backwards (most recent first). Returns the slot index or `-1` (not found = global). The slot index becomes the operand for `OP_GET_LOCAL`/`OP_SET_LOCAL`.

### Upvalues (Closures)

```pascal
function resolveUpvalue(compiler: pCompiler; const name: TToken): integer;
```

Recursive resolution:
1. Try `resolveLocal` in the enclosing compiler
2. If found, mark it as captured (`isCaptured := true`), add upvalue with `isLocal = true`
3. If not found locally, try `resolveUpvalue` in the enclosing compiler (recursive)
4. If found, add upvalue with `isLocal = false` (capturing another upvalue)

## Control Flow Compilation

### `if` Statement

```
  [condition]
  OP_JUMP_IF_FALSE → elseClause
  OP_POP
  [then body]
  OP_JUMP → end
elseClause:
  OP_POP
  [else body]  (if present)
end:
```

### `while` Statement

```
loopStart:
  [condition]
  OP_JUMP_IF_FALSE → exit
  OP_POP
  [body]
  OP_LOOP → loopStart
exit:
  OP_POP
```

### `for` Statement

Desugars into a while loop with an initializer and increment:

```
  [initializer]
loopStart:
  [condition]
  OP_JUMP_IF_FALSE → exit
  OP_POP
  OP_JUMP → body
increment:
  [increment expression]
  OP_POP
  OP_LOOP → loopStart
body:
  [body statement]
  OP_LOOP → increment
exit:
  OP_POP
```

### `and` / `or`

Short-circuit via jumps:
- `and_`: Jump over right operand if left is falsey
- `or_`: Jump over right operand if left is truthy (using a double-jump pattern)

## Function Compilation

`functionBody(funcType)`:
1. Push a new `TCompiler` onto the compiler chain
2. Begin a new scope
3. Parse parameters, adding each as a local
4. Parse the body (`block`)
5. Emit `OP_NIL` + `OP_RETURN` (implicit nil return)
6. Pop the compiler, restore `Current`
7. Emit `OP_CLOSURE` (or `OP_CLOSURE_LONG`) referencing the function constant
8. For each upvalue, emit `isLocal` flag + index

## Record Declaration

`recordDeclaration()`:
1. Parse the record name → global variable
2. Parse field names in parentheses
3. Store each field name as a string constant
4. Emit `OP_RECORD` with field count + field name indices + type name index
5. Define the global (the record type constructor lives in a variable)

## Error Recovery

`synchronize()` — on error, skip tokens until a statement boundary (`;` or keyword that starts a statement). `PanicMode` suppresses cascading errors until synchronization.

## GC Safety in the Compiler

Several compiler operations allocate (creating strings for constants, growing arrays). To prevent the GC from collecting in-progress objects:

```pascal
// Push string onto stack to protect from GC until stored in constants.
strObj := CreateString(lexeme, VM.MemTracker);
pushStack(VM.Stack, StringToValue(strObj), VM.MemTracker);
// ... use strObj ...
popStack(VM.Stack);
```

This "push to protect" pattern appears throughout the compiler.
