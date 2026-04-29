# Native Functions

Native functions bridge the gap between Lox scripts and the Delphi host. They give Lox code access to capabilities that cannot be expressed in Lox itself — timers, memory introspection, data structures, and Delphi object wrappers.

## Type Definitions

### TNativeFn

```pascal
TNativeFn = function(argCount: integer; args: pValue): TValue;
```

Every native function follows this signature:

| Parameter | Meaning |
|-----------|---------|
| `argCount` | Number of arguments the caller passed |
| `args` | Pointer to the first argument on the VM stack (`args[0]`, `args[1]`, …) |
| **Return** | A `TValue` pushed back onto the stack as the call result |

### TObjNative

```pascal
TObjNative = record
  Obj  : TObj;       // GC object header (ObjectKind = okNative)
  func : TNativeFn;  // the function pointer
end;
```

A thin GC-managed wrapper around a `TNativeFn`. The `Obj` header lets the garbage collector track the value like any other heap object.

## Registration

All native functions are registered during `InitVM`:

```pascal
procedure defineNative(const name : AnsiString; func : TNativeFn);
```

`defineNative` does three things:

1. **Creates a string key** — calls `CreateString` to intern the function name.
2. **Wraps the function** — calls `newNative(func)` to allocate a `TObjNative`.
3. **Stores in globals** — adds the pair to `VM.Globals` so Lox code can look up the name like any global variable.

Both the string and the native object are pushed onto the stack during creation to protect them from the garbage collector:

```pascal
pushStack(VM.Stack, StringToValue(nameStr), VM.MemTracker);
native := newNative(func, VM.MemTracker);
pushStack(VM.Stack, CreateObject(pObj(native)), VM.MemTracker);
TableSet(VM.Globals, nameStr, CreateObject(pObj(native)), VM.MemTracker);
popStack(VM.Stack);
popStack(VM.Stack);
```

After `TableSet` the globals table holds a strong reference, so the stack temporaries are popped.

## Runtime Dispatch

When the VM encounters a call and the callee is a native function, `CallValue` handles it inline — no call frame is pushed.

```
1. Resolve callee → isNative(callee)?
2. Extract function pointer: native := pObjNative(callee.ObjValue)^.func
3. Compute args pointer: StackTop - argCount * SizeOf(TValue)
4. Call: nativeResult := native(argCount, argsPointer)
5. Check VM.RuntimeErrorStr (native can signal errors)
6. Pop argCount + 1 values (args + callee slot)
7. Push nativeResult
```

Because no `CallFrame` is created, native calls are cheaper than Lox function calls — no closure allocation, no IP save/restore.

### Error Reporting

Native functions signal errors by calling `RuntimeError(msg)`, which sets `VM.RuntimeErrorStr`. After every native call the VM checks this field:

```pascal
if VM.RuntimeErrorStr <> '' then
begin
  Result := false;
  Exit;
end;
```

If set, the VM unwinds normally (no exception mechanism is used).

## Built-in Native Functions

### System

| Function | Arity | Returns | Description |
|----------|-------|---------|-------------|
| `clock()` | 0 | number | Wall-clock time in seconds via `GetTickCount / 1000.0` |
| `collectGarbage()` | 0 | nil | Forces a garbage-collection cycle |
| `assert(cond [, msg])` | 1–2 | nil | Runtime error if `cond` is falsey; optional message string |
| `bytesAllocated()` | 0 | number | Current tracked memory in bytes |
| `objectsAllocated()` | 0 | number | Count of live GC-tracked objects |

#### clock

```pascal
function clockNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(GetTickCount / 1000.0);
end;
```

The original *Crafting Interpreters* native. Uses the Windows `GetTickCount` API.

#### assert

```pascal
function assertNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount < 1) or (argCount > 2) then
  begin
    RuntimeError('assert() takes 1 or 2 arguments.');
    Result := CreateNilValue;
    Exit;
  end;
  if IsFalsey(args[0]) then
  begin
    if (argCount = 2) and IsString(args[1]) then
      RuntimeError('Assertion failed: ' + ...)
    else
      RuntimeError('Assertion failed.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNilValue;
end;
```

Accepts an optional second argument for a custom error message. Both `nil` and `false` are falsey.

#### bytesAllocated / objectsAllocated

Introspection helpers primarily used in test scripts to verify GC behavior. `objectsAllocated` walks the full `CreatedObjects` linked list to produce a count.

### Arrays

Arrays are not a language-level type with dedicated syntax. Instead, they are accessed entirely through native functions.

| Function | Arity | Returns | Description |
|----------|-------|---------|-------------|
| `newArray()` | 0 | array | Create an empty `TObjArray` |
| `arrayPush(arr, val)` | 2 | array | Append value; returns the array (enables chaining) |
| `arrayPop(arr)` | 1 | value | Remove and return the last element |
| `arrayGet(arr, idx)` | 2 | value | Read element at index |
| `arraySet(arr, idx, val)` | 3 | value | Write element at index; returns the new value |
| `arrayLen(arr)` | 1 | number | Current element count |
| `arrayRemove(arr, idx)` | 2 | value | Remove at index (shifts elements left), returns removed value |

#### Argument Validation Pattern

Every array function follows the same pattern:

```pascal
function arrayPushNative(argCount: integer; args: pValue): TValue;
begin
  // 1. Check arity
  if argCount <> 2 then
  begin
    RuntimeError('arrayPush() takes 2 arguments (array, value).');
    Result := CreateNilValue;
    Exit;
  end;
  // 2. Check type of first argument
  if not isArray(args[0]) then
  begin
    RuntimeError('First argument to arrayPush() must be an array.');
    Result := CreateNilValue;
    Exit;
  end;
  // 3. Perform operation
  arr := pObjArray(args[0].ObjValue);
  ...
end;
```

1. Validate argument count.
2. Validate argument types (first argument must be an array; indices must be numbers).
3. For indexed access, validate bounds.
4. Perform the operation.
5. On any validation failure, call `RuntimeError` and return `CreateNilValue`.

#### Array Growth

`arrayPush` manages capacity internally:

- Initial capacity: 0
- First push allocates 8 slots
- Subsequent growth: capacity × `GROWTH_FACTOR` (2)
- Memory resized through `Allocate`, which is tracked by the GC memory tracker

#### Usage

```lox
var a = newArray();
arrayPush(a, 10);
arrayPush(a, 20);
arrayPush(a, 30);
assert(arrayLen(a) == 3);
assert(arrayGet(a, 0) == 10);

arraySet(a, 1, 99);
assert(arrayGet(a, 1) == 99);

var last = arrayPop(a);    // 30
var mid = arrayRemove(a, 0); // 10 — shifts 99 left
```

## Native Objects (Delphi FFI)

Native objects wrap arbitrary Delphi objects so Lox scripts can call methods on them. This is a more structured system layered on top of the basic native function mechanism.

### Type Hierarchy

```
TNativeMethodFn = function(instance: Pointer; argCount: integer; args: pValue): TValue;

TNativeMethod = record
  name : AnsiString;       // method name visible to Lox
  fn   : TNativeMethodFn;  // Delphi function pointer
end;

TNativeClassInfo = record
  name        : AnsiString;               // class name
  methods     : array of TNativeMethod;    // method table
  destructor_ : TNativeDestructor;         // cleanup callback (may be nil)
end;

TObjNativeObject = record
  Obj       : TObj;              // GC header (ObjectKind = okNativeObject)
  instance  : Pointer;           // opaque pointer to the Delphi object
  classInfo : pNativeClassInfo;  // pointer to the class registry entry
end;
```

`TNativeMethodFn` differs from `TNativeFn` by receiving the `instance` pointer as its first parameter.

### Registration

```pascal
registerNativeClass('StringList', slMethods, SL_Destroy);
defineNative('StringList', stringListNative);
```

Two steps:

1. **`registerNativeClass`** — stores a `TNativeClassInfo` (name, method table, destructor) in the global `NativeClassRegistry` array.
2. **`defineNative`** — registers a constructor function in `VM.Globals`. The constructor creates the Delphi object and wraps it in `TObjNativeObject`:

```pascal
function stringListNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 0 then
  begin
    RuntimeError('StringList() takes no arguments.');
    Exit(CreateNilValue);
  end;
  classInfo := findNativeClass('StringList');
  obj := newNativeObject(TStringList.Create, classInfo, VM.MemTracker);
  Result := CreateObject(pObj(obj));
end;
```

### Method Dispatch (OP_INVOKE)

When Lox code calls `sl.add("hello")`, the compiler emits `OP_INVOKE` with the method name index and argument count. At runtime:

```
1. Read method-name index and arg count from bytecode
2. Peek at receiver (below args on stack)
3. Verify receiver is a native object
4. Look up method name in classInfo.methods (linear scan via findNativeMethod)
5. Call: nativeMethod(nativeObj^.instance, argCount, argsPointer)
6. Check VM.RuntimeErrorStr
7. Pop args + receiver, push result
```

`OP_INVOKE_LONG` is the 24-bit operand variant for programs with more than 256 constants.

### findNativeMethod

```pascal
function findNativeMethod(classInfo : pNativeClassInfo;
  const methodName : AnsiString; out found : TNativeMethodFn) : boolean;
var i : integer;
begin
  for i := 0 to High(classInfo^.methods) do
    if classInfo^.methods[i].name = methodName then
    begin
      found := classInfo^.methods[i].fn;
      Exit(True);
    end;
  found := nil;
  Result := False;
end;
```

Linear scan over the method table. For the small method counts typical of native classes, this is effectively constant time.

### GC Integration

`TObjNativeObject` is tracked by the garbage collector like any other heap object. However, the wrapped Delphi object (`instance: Pointer`) is **opaque to the GC** — it is not traced or marked.

When the GC sweeps a native object, it calls the registered destructor:

```pascal
okNativeObject : begin
  if Assigned(pObjNativeObject(obj)^.classInfo^.destructor_) then
    pObjNativeObject(obj)^.classInfo^.destructor_(pObjNativeObject(obj)^.instance);
  Allocate(Pointer(obj), SizeOf(TObjNativeObject), 0, vm.MemTracker);
end;
```

This ensures the Delphi-side resource is freed exactly once, at the same time the Lox wrapper is collected.

### Printing

`ValueToStr` renders native objects as `<ClassName>`:

```pascal
okNativeObject: Result := '<' + String(pObjNativeObject(value.ObjValue)^.classInfo^.name) + '>';
```

### Built-in: StringList

A wrapper around Delphi's `TStringList`.

| Method | Arity | Arg Types | Returns | Description |
|--------|-------|-----------|---------|-------------|
| `add(str)` | 1 | string | nil | Append string to list |
| `get(idx)` | 1 | number | string | Read item at index (bounds-checked) |
| `count()` | 0 | — | number | Number of items |
| `remove(idx)` | 1 | number | nil | Delete item at index (bounds-checked) |

#### Usage

```lox
var sl = StringList();
sl.add("alpha");
sl.add("beta");
print sl.count();     // 2
print sl.get(0);      // "alpha"
sl.remove(0);
print sl.count();     // 1
print sl.get(0);      // "beta"
```

## Writing a New Native Function

### Simple Function

1. Write a function matching `TNativeFn`:

```pascal
function myNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('myNative() takes 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  // ... do work with args[0] ...
  Result := CreateNumber(42);
end;
```

2. Register in `InitVM`:

```pascal
defineNative('myNative', myNative);
```

### Native Object Class

1. Define method functions matching `TNativeMethodFn`.
2. Optionally define a destructor matching `TNativeDestructor`.
3. Build a method table and register:

```pascal
var methods: array[0..1] of TNativeMethod;
methods[0].name := 'doSomething';  methods[0].fn := MyClass_DoSomething;
methods[1].name := 'getValue';     methods[1].fn := MyClass_GetValue;
registerNativeClass('MyClass', methods, MyClass_Destroy);
```

4. Write a constructor function that creates the Delphi object and wraps it:

```pascal
function myClassNative(argCount: integer; args: pValue): TValue;
begin
  classInfo := findNativeClass('MyClass');
  obj := newNativeObject(TMyClass.Create, classInfo, VM.MemTracker);
  Result := CreateObject(pObj(obj));
end;
```

5. Register the constructor:

```pascal
defineNative('MyClass', myClassNative);
```

## Summary

| Concept | Key Type / Function | Location |
|---------|-------------------|----------|
| Function signature | `TNativeFn` | Line 256 |
| Function wrapper | `TObjNative` | Line 258 |
| Registration | `defineNative` | Line 3731 |
| Call dispatch | `CallValue` (isNative branch) | Line 2416 |
| Method signature | `TNativeMethodFn` | Line 297 |
| Class registry | `TNativeClassInfo` | Line 304 |
| Object wrapper | `TObjNativeObject` | Line 312 |
| Class registration | `registerNativeClass` | Line 4014 |
| Method dispatch | `OP_INVOKE` handler | Line 2908 |
| Method lookup | `findNativeMethod` | Line 1785 |
| GC cleanup | `FreeObject` (okNativeObject) | Line 3379 |
