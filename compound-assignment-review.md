# Compound Assignment Operators — Review

## Summary

Added `+=`, `-=`, `*=`, `/=` as syntactic sugar. `x += 5` desugars to the equivalent of `x = x + 5` at the bytecode level (GET, expression, arithmetic op, SET).

## What Works

| Syntax | Example | Status |
|--------|---------|--------|
| Local variable | `var x = 10; x += 5;` | ✅ Works |
| Global variable | `g -= 25;` (inside function) | ✅ Works |
| Upvalue (closure) | `fun f() { x *= 2; }` | ✅ Works |
| Fast-slot locals (0–7) | Most local vars | ✅ Works |
| Long globals (>255 constants) | Large scripts | ✅ Works |
| String concatenation | `s += " world";` | ✅ Works |
| In loops | `while (...) { sum += i; i += 1; }` | ✅ Works |

## What Doesn't Work (Yet)

| Syntax | Example | Status | Blocker |
|--------|---------|--------|---------|
| Record field | `p.x += 5;` | ❌ Compile error | Needs `OP_DUP` |
| Dict subscript | `d["key"] += 1;` | ❌ Compile error | Needs `OP_DUP` × 2 |
| Array subscript | `a[0] += 1;` | ❌ Compile error | Needs `OP_DUP` × 2 |

### Why It Doesn't Work on Properties/Subscripts

The `dot_` and `subscript_` handlers need the object (and index) on the stack for both the GET and the SET. A simple `OP_DUP` opcode (duplicate top-of-stack) would unblock `obj.field += expr`. Array/dict subscript is slightly harder — both the object and the index need duplicating.

### Proposed Fix (Future)

1. Add `OP_DUP` — pushes a copy of the top of stack
2. For `obj.field += expr`: emit `DUP, GET_PROPERTY, expr, OP_ADD, SET_PROPERTY`
3. For `a[i] += expr`: emit `DUP, DUP` (object twice), `expr_index, DUP` to keep index... (more complex, may warrant a dedicated `OP_COMPOUND_SUBSCRIPT` or a different approach)

## Type Safety

Compound assignment hits the same runtime type checks as the underlying arithmetic operator:

| Expression | Runtime Error |
|-----------|---------------|
| `string *= 3` | "Operands must be numbers." |
| `dict *= 2` | "Operands must be numbers." |
| `array += 1` | "Operands must be two numbers or two strings." |
| `bool += 1` | "Operands must be two numbers or two strings." |
| `nil += 5` | "Operands must be two numbers or two strings." |
| `record -= 1` | "Operands must be numbers." |

No new error handling was needed — existing arithmetic operator guards catch all invalid types.

## Implementation Details

### Scanner Changes
- Added 4 tokens: `TOKEN_PLUS_EQUAL`, `TOKEN_MINUS_EQUAL`, `TOKEN_STAR_EQUAL`, `TOKEN_SLASH_EQUAL`
- Scanner checks for `=` after `+`, `-`, `*`, `/`
- Comment handling (`//`, `/* */`) still works — it's handled in whitespace skipping before the main character switch

### Compiler Changes
- `namedVariable()` now checks for compound assignment tokens after checking for `=`
- A nested `MatchCompoundAssign()` function returns the corresponding arithmetic opcode
- Fast-slot path (locals 0–7) and general path both support it
- Parse rules table extended with 4 `PREC_NONE` entries for the new tokens

### Bytecode Emitted

For `x += expr` (local slot 3):
```
OP_GET_LOCAL_3
<expr bytecode>
OP_ADD
OP_SET_LOCAL_3
```

For `g += expr` (global):
```
OP_GET_GLOBAL <idx>
<expr bytecode>
OP_ADD
OP_SET_GLOBAL <idx>
```

No new opcodes were added. The desugaring uses existing GET/SET/arithmetic ops.

## Discussion Points

1. **Should we add `OP_DUP` to enable `obj.field += expr`?** It's one small opcode addition but opens up property compound assignment.

2. **`%=` (modulo-assign)?** We have `TOKEN_PERCENT` / `OP_MODULO` — adding `%=` is trivial if desired.

3. **`++` / `--` increment/decrement?** These are more complex (prefix vs postfix, expression vs statement). Not recommended unless there's demand.

4. **Should compound assignment be an expression or a statement?** Currently it's neither — it's handled inside `namedVariable` which is itself part of an expression. The result left on the stack is the new value (same as regular assignment). This matches C/JS semantics: `print (x += 5)` would print the new value.

## Test Coverage

- `test/assignment/compound_assignment.lox` — 12 passing tests covering all operators, types, loops, closures
- `test/assignment/compound_assign_type_error.lox` — string *= number
- `test/assignment/compound_assign_dict_error.lox` — dict *= number
- `test/assignment/compound_assign_array_error.lox` — array += number
- `test/assignment/compound_assign_record_error.lox` — record -= number
- `test/assignment/compound_assign_bool_error.lox` — bool += number
- `test/assignment/compound_assign_nil_error.lox` — nil += number
