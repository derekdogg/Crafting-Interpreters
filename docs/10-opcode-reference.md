# Opcode Reference

Complete listing of all 42 opcodes with encoding and stack effects.

## Encoding Key

- **1B** = 1-byte operand
- **3B** = 3-byte little-endian operand (24-bit)
- **2B** = 2-byte big-endian operand (16-bit jump offset)

## Constants & Literals

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 0 | `OP_CONSTANT` | 1B (const idx) | → value | Push constant |
| 14 | `OP_CONSTANT_LONG` | 3B (const idx) | → value | Push constant (24-bit index) |
| 7 | `OP_TRUE` | — | → true | Push boolean true |
| 8 | `OP_FALSE` | — | → false | Push boolean false |
| 9 | `OP_NIL` | — | → nil | Push nil |

## Arithmetic

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 1 | `OP_NEGATE` | — | number → number | Negate top |
| 2 | `OP_ADD` | — | a, b → result | Add (numbers or string concat) |
| 3 | `OP_SUBTRACT` | — | a, b → result | Subtract numbers |
| 4 | `OP_MULTIPLY` | — | a, b → result | Multiply numbers |
| 5 | `OP_DIVIDE` | — | a, b → result | Divide numbers |
| 30 | `OP_MODULO` | — | a, b → result | Modulo (truncated) |

## Comparison & Equality

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 11 | `OP_EQUAL` | — | a, b → bool | Deep equality |
| 12 | `OP_GREATER` | — | a, b → bool | a > b (numbers) |
| 13 | `OP_LESS` | — | a, b → bool | a < b (numbers) |
| 10 | `OP_NOT` | — | value → bool | Logical not (falsey → true) |

Note: `!=`, `>=`, `<=` are compiled as combinations:
- `!=` → `OP_EQUAL` + `OP_NOT`
- `>=` → `OP_LESS` + `OP_NOT`
- `<=` → `OP_GREATER` + `OP_NOT`

## Stack Management

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 16 | `OP_POP` | — | value → | Discard top |
| 15 | `OP_PRINT` | — | value → | Pop and print |

## Global Variables

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 17 | `OP_DEFINE_GLOBAL` | 1B (name idx) | value → | Define global from top |
| 18 | `OP_GET_GLOBAL` | 1B (name idx) | → value | Push global's value |
| 19 | `OP_SET_GLOBAL` | 1B (name idx) | value → value | Assign (keep on stack) |
| 31 | `OP_DEFINE_GLOBAL_LONG` | 3B (name idx) | value → | 24-bit variant |
| 32 | `OP_GET_GLOBAL_LONG` | 3B (name idx) | → value | 24-bit variant |
| 33 | `OP_SET_GLOBAL_LONG` | 3B (name idx) | value → value | 24-bit variant |

## Local Variables

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 20 | `OP_GET_LOCAL` | 1B (slot) | → value | Push local from stack slot |
| 21 | `OP_SET_LOCAL` | 1B (slot) | value → value | Write top to stack slot |

## Upvalues (Closures)

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 27 | `OP_GET_UPVALUE` | 1B (idx) | → value | Push captured variable |
| 28 | `OP_SET_UPVALUE` | 1B (idx) | value → value | Write to captured variable |
| 29 | `OP_CLOSE_UPVALUE` | — | value → | Close upvalue, pop slot |

## Control Flow

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 22 | `OP_JUMP` | 2B (offset) | — | Unconditional forward jump |
| 23 | `OP_JUMP_IF_FALSE` | 2B (offset) | (peek) | Jump if top is falsey (no pop) |
| 24 | `OP_LOOP` | 2B (offset) | — | Unconditional backward jump |

## Functions

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 25 | `OP_CALL` | 1B (argCount) | callee, args... → result | Call function/native/record |
| 6 | `OP_RETURN` | — | value → | Return from current frame |
| 26 | `OP_CLOSURE` | 1B (const) + N×2B | → closure | Create closure |
| 34 | `OP_CLOSURE_LONG` | 3B (const) + N×2B | → closure | Create closure (24-bit) |

`OP_CLOSURE` / `OP_CLOSURE_LONG` are followed by pairs of bytes for each upvalue:
- Byte 1: `1` = local, `0` = upvalue from enclosing
- Byte 2: slot/upvalue index

## Records

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 35 | `OP_RECORD` | 1B (count) + N×1B (fields) + 1B (name) | → recordType | Define record type |
| 36 | `OP_GET_PROPERTY` | 1B (name idx) | record → value | Read field |
| 37 | `OP_SET_PROPERTY` | 1B (name idx) | record, value → value | Write field |
| 40 | `OP_GET_PROPERTY_LONG` | 3B (name idx) | record → value | 24-bit variant |
| 41 | `OP_SET_PROPERTY_LONG` | 3B (name idx) | record, value → value | 24-bit variant |

## Method Invocation

| # | Opcode | Operands | Stack Effect | Description |
|---|--------|----------|-------------|-------------|
| 38 | `OP_INVOKE` | 1B (name idx) + 1B (argc) | receiver, args... → result | Call native method |
| 39 | `OP_INVOKE_LONG` | 3B (name idx) + 1B (argc) | receiver, args... → result | 24-bit variant |
