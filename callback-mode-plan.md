# Callback Mode — Minimal Implementation Plan

## Overview

Support a host-driven game loop where the Delphi host calls named Lox functions (`update`, `draw`, `keypressed`, etc.) each frame, instead of the script owning a `while` loop.

Existing scripts (with their own `while`/`processMessages` loop) continue working unchanged.

---

## Change 1: `RunUntilFrame` — re-entrant dispatch

**File:** `Chunk_Types.pas`

Currently `Run` loops until `FrameCount = 0`. Add a variant that stops when the frame count drops to a caller-specified baseline:

```pascal
function RunUntilFrame(baselineFrameCount: Integer): TInterpretResult;
```

The only difference from `Run`: the `OP_RETURN` handler checks:

```pascal
OP_RETURN: begin
  // ...existing frame teardown...
  Dec(VM.FrameCount);
  if VM.FrameCount <= baselineFrameCount then
  begin
    // We've returned from the function the host called — stop.
    Result.code := INTERPRET_OK;
    Exit;
  end;
  // ...rebase frame, continue dispatch...
end;
```

`Run` itself becomes `RunUntilFrame(0)` — zero behaviour change for existing paths.

---

## Change 2: `CallLoxGlobal` — call a named function from Delphi

**File:** `Chunk_Types.pas` (public API)

```pascal
function CallLoxGlobal(const name: AnsiString; 
                       const args: array of TValue;
                       out returnVal: TValue): TInterpretResult;
```

Implementation:

1. Look up `name` in the VM globals table. If not found or not a closure, return error.
2. Check arity matches `Length(args)`.
3. Push the closure value onto the stack.
4. Push each arg onto the stack.
5. Set up a new call frame (same logic as `OP_CALL`'s closure fast-path).
6. Call `RunUntilFrame(VM.FrameCount - 1)`.
7. Pop the return value from the stack.

---

## Change 3: `GlobalExists` — check if a global is defined

**File:** `Chunk_Types.pas` (public API)

```pascal
function GlobalExists(const name: AnsiString): Boolean;
```

Looks up `name` in the globals table. Used by the host to detect callback mode.

---

## Change 4: Host-driven game loop

**File:** `fmGame.pas`

After `CompileAndRun` returns, check for callback mode:

```pascal
function TfrmGame.RunScript(const Source: AnsiString): TInterpretResult;
begin
  NotifyScriptStarted;
  InitVM;
  try
    // Register natives as before
    defineNative('processMessages', processMessagesNative, 0);
    RegisterCanvasNatives;
    RegisterSoundNatives;
    // ...

    // Run top-level code (setup, global function definitions)
    Result := CompileAndRun(PAnsiChar(Source));
    if Result.code <> INTERPRET_OK then Exit;

    // Detect callback mode
    if GlobalExists('draw') then
      Result := RunCallbackLoop;
  finally
    FreeVM;
    StopAllSound;
    NotifyScriptStopped;
  end;
end;
```

The callback loop:

```pascal
function TfrmGame.RunCallbackLoop: TInterpretResult;
var
  dt: Double;
  lastTime, now: Double;
  dummy: TValue;
  evt, key: string;
  btn, lx, ly: Integer;
begin
  lastTime := GetHighResTime;
  FAbortScript := False;

  while not FAbortScript and not Form4.FClosing do
  begin
    Application.ProcessMessages;
    if FAbortScript then Break;

    now := GetHighResTime;
    dt := now - lastTime;
    if dt > 0.1 then dt := 0.1;
    lastTime := now;

    // Dispatch input events as callbacks
    while FEventQueue.HasNext do
    begin
      evt := FEventQueue.Dequeue;
      if ParseKeyEvent(evt, 'keydown', key) then
      begin
        if GlobalExists('keypressed') then
          CallLoxGlobal('keypressed', [CreateStringValue(key)], dummy);
      end
      else if ParseKeyEvent(evt, 'keyup', key) then
      begin
        if GlobalExists('keyreleased') then
          CallLoxGlobal('keyreleased', [CreateStringValue(key)], dummy);
      end
      else if ParseMouseEvent(evt, 'mousedown', btn, lx, ly) then
      begin
        if GlobalExists('mousepressed') then
          CallLoxGlobal('mousepressed', 
            [CreateNumber(lx), CreateNumber(ly), CreateNumber(btn)], dummy);
      end;
      // ... mousemoved, mouseup similarly
    end;

    // update(dt)
    if GlobalExists('update') then
    begin
      Result := CallLoxGlobal('update', [CreateNumber(dt)], dummy);
      if Result.code <> INTERPRET_OK then Exit;
    end;

    // draw()
    if GlobalExists('draw') then
    begin
      Result := CallLoxGlobal('draw', [], dummy);
      if Result.code <> INTERPRET_OK then Exit;
    end;

    // Frame pacing (vsync or sleep)
    Sleep(1);
  end;

  Result.code := INTERPRET_OK;
end;
```

---

## Change 5: VM state survives between calls

This already works — `InitVM` / `FreeVM` bracket the whole session. Globals, the GC heap, and the stack all persist. The only thing to ensure:

- After `CompileAndRun` returns, the stack should be clean (just the script-level return value). `CompileAndRun` already pops everything via `OP_RETURN` from frame 0.
- `CallLoxGlobal` must leave the stack clean after each call (push closure+args, run, pop return value).

---

## Summary of touchpoints

| File | Change |
|------|--------|
| `Chunk_Types.pas` | Add `RunUntilFrame`, `CallLoxGlobal`, `GlobalExists` |
| `fmGame.pas` | Add `RunCallbackLoop`, detection logic after `CompileAndRun` |

No changes to the compiler, scanner, or bytecode format. No new opcodes. Existing scripts work unchanged — they run their own loop inside `CompileAndRun` and `GlobalExists('draw')` returns false afterwards.

---

## Optional nice-to-haves (not required)

- **`quit()` native** — sets a flag that breaks the callback loop (replaces `running = false`).
- **`load()` / `init()` callback** — called once after top-level, before the first frame.
- **Frame rate target** — host caps to 60fps with proper sleep/vsync instead of `Sleep(1)`.
- **`GlobalExists` caching** — look up the closures once at loop entry, not every frame.
