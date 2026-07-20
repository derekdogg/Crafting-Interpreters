unit EventNatives;

// ============================================================
// Native functions for the Lox event engine.
// Registers: onKeyPressed, onKeyReleased, onKeyHeld,
//            onMouseDown, onMouseUp, onMouseMove, onClick,
//            processEvents,
//            onFrame, runGameLoop, stopGameLoop
// Also provides simulation natives for scripted testing:
//            simulateKeyDown, simulateKeyUp,
//            simulateMouseDown, simulateMouseUp, simulateMouseMove
//
// Named-target support
// --------------------
// All callback registration natives accept an optional leading
// string argument naming the target VCL control:
//   onMouseDown(fun(btn,x,y) { ... })            -- canvas/form default
//   onMouseDown("btnSave", fun(btn,x,y) { ... })  -- specific control
// onClick always requires a control name:
//   onClick("btnSave", fun() { ... })
// ============================================================

interface

uses
  Winapi.Windows, Suto;

procedure RegisterEventNatives;

implementation

uses
  Winapi.DwmApi, Vcl.Forms, System.SysUtils, System.Classes, LoxEventEngine,
  LoxCanvas;

// --- Helper: parse optional (controlName, closure) or (closure) ---
// Returns True if args are valid; sets controlName and closure.

function ParseCallbackArgs(const NativeName: string; argCount: Integer;
  args: pValue; out controlName: string; out closure: TValue): Boolean;
begin
  Result := False;
  if (argCount = 1) and isClosure(args[0]) then
  begin
    controlName := '';
    closure := args[0];
    Result := True;
  end
  else if (argCount = 2) and isString(args[0]) and isClosure(args[1]) then
  begin
    controlName := string(AsAnsiString(args[0]));
    closure := args[1];
    Result := True;
  end
  else
    RuntimeError(NativeName + '() takes an optional control name and 1 function argument.');
end;

// --- Callback registration natives ---

function onKeyPressedNative(argCount: integer; args: pValue): TValue;
var
  controlName: string;
  closure: TValue;
begin
  if ParseCallbackArgs('onKeyPressed', argCount, args, controlName, closure) then
    ActiveEngine.SetCallback(eckKeyPressed, controlName, closure);
  Result := CreateNilValue;
end;

function onKeyReleasedNative(argCount: integer; args: pValue): TValue;
var
  controlName: string;
  closure: TValue;
begin
  if ParseCallbackArgs('onKeyReleased', argCount, args, controlName, closure) then
    ActiveEngine.SetCallback(eckKeyReleased, controlName, closure);
  Result := CreateNilValue;
end;

function onKeyHeldNative(argCount: integer; args: pValue): TValue;
var
  closure: TValue;
begin
  // onKeyHeld only supports canvas-level (no named target) because
  // FHeldKeys does not track sender — dispatch always uses empty ControlName.
  if (argCount = 1) and isClosure(args[0]) then
  begin
    closure := args[0];
    ActiveEngine.SetCallback(eckKeyHeld, '', closure);
  end
  else
    RuntimeError('onKeyHeld() takes exactly 1 function argument (named targets not supported).');
  Result := CreateNilValue;
end;

function onMouseDownNative(argCount: integer; args: pValue): TValue;
var
  controlName: string;
  closure: TValue;
begin
  if ParseCallbackArgs('onMouseDown', argCount, args, controlName, closure) then
    ActiveEngine.SetCallback(eckMouseDown, controlName, closure);
  Result := CreateNilValue;
end;

function onMouseUpNative(argCount: integer; args: pValue): TValue;
var
  controlName: string;
  closure: TValue;
begin
  if ParseCallbackArgs('onMouseUp', argCount, args, controlName, closure) then
    ActiveEngine.SetCallback(eckMouseUp, controlName, closure);
  Result := CreateNilValue;
end;

function onMouseMoveNative(argCount: integer; args: pValue): TValue;
var
  controlName: string;
  closure: TValue;
begin
  if ParseCallbackArgs('onMouseMove', argCount, args, controlName, closure) then
    ActiveEngine.SetCallback(eckMouseMove, controlName, closure);
  Result := CreateNilValue;
end;

function onClickNative(argCount: integer; args: pValue): TValue;
var
  controlName: string;
  closure: TValue;
begin
  // onClick always requires a control name: onClick("btnSave", fun() { ... })
  if (argCount = 2) and isString(args[0]) and isClosure(args[1]) then
  begin
    controlName := string(AsAnsiString(args[0]));
    closure := args[1];
    ActiveEngine.SetCallback(eckClick, controlName, closure);
  end
  else
    RuntimeError('onClick() takes a control name and 1 function argument.');
  Result := CreateNilValue;
end;

function processEventsNative(argCount: integer; args: pValue): TValue;
begin
  if ActiveEngine.Dispatching then
  begin
    RuntimeError('processEvents() cannot be called from inside an event callback.');
    Exit(CreateNilValue);
  end;
  if ActiveEngine.GameLoopActive then
  begin
    RuntimeError('processEvents() cannot be called while runGameLoop() is active; the loop already pumps events every frame.');
    Exit(CreateNilValue);
  end;
  // Flush print output
  ActiveEngine.FlushOutput;
  // Reset per-frame edge state (typed chars, key/mouse press+release
  // edges) before the pump so poll natives and the widget engine see
  // only transitions that occur during THIS processEvents() tick \u2014
  // matching runGameLoop's contract. Without this a manual loop like
  //   while (running) processEvents();
  // would accumulate WM_CHAR text forever and leave click/nav edges
  // stuck true after their originating frame.
  ClearFrameEdges;
  // Pump Windows messages (delivers key/mouse to VCL handlers)
  Application.ProcessMessages;
  // Dispatch any pending events as callbacks
  ActiveEngine.DispatchPendingEvents;
  // Yield a millisecond so `while (running) processEvents();` doesn't peg a
  // CPU core. Hold the 1ms timer resolution only for the sleep itself so
  // hand-rolled frame loops stay smooth without the process keeping the
  // system timer fast while idle.
  AcquireHighResTiming;
  try
    TThread.Sleep(1);
  finally
    ReleaseHighResTiming;
  end;
  // Check for abort (engine stopped or host app closing)
  if not ActiveEngine.Running then
  begin
    RuntimeError('Script aborted: window closed.');
    Exit(CreateNilValue);
  end;
  if Assigned(ActiveEngine.OnCheckAbort) and ActiveEngine.OnCheckAbort() then
  begin
    RuntimeError('Script aborted: application closing.');
    Exit(CreateNilValue);
  end;
  Result := CreateNilValue;
end;

// --- Engine-owned game loop ---

function onFrameNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount = 1) and isClosure(args[0]) then
    ActiveEngine.SetCallback(eckFrame, '', args[0])
  else
    RuntimeError('onFrame() takes exactly 1 function argument.');
  Result := CreateNilValue;
end;

function stopGameLoopNative(argCount: integer; args: pValue): TValue;
begin
   if argCount <> 0 then
   begin
     RuntimeError('stopGameLoop() takes no arguments.');
     Exit(CreateNilValue);
   end;
  ActiveEngine.GameLoopActive := False;
  Result := CreateNilValue;
end;

// runGameLoop([targetFps]) -> nil
// Runs the engine-owned frame loop until stopGameLoop() is called or the
// host aborts. Each frame: flush output, clear per-frame poll edges, pump
// Windows messages, dispatch event callbacks, then invoke onFrame(dt) with
// the elapsed seconds since the previous frame (clamped to 0.1s so debugger
// pauses / window drags don't produce physics jumps). targetFps defaults to
// 0 (uncapped); pass a positive number to cap instead. Pacing uses an
// absolute QPC deadline per frame (no drift accumulation): coarse
// TThread.Sleep(1) while >2ms remain — the loop holds a scoped
// timeBeginPeriod(1) while active so that really is ~1ms — then yields
// until the deadline. If a frame overruns, the deadline resets rather than
// fast-forwarding to catch up.
//
// IMPORTANT for canvas scripts: present() ends with DwmFlush, i.e. it is
// already vsynced to the compositor. A script that presents every frame
// should run UNCAPPED (the default — call runGameLoop() with no argument,
// or runGameLoop(0) explicitly) and scale movement by dt — the compositor
// paces the loop at the monitor's true refresh and dt stays steady. Passing
// a fixed target (e.g. 60) adds a second, unsynchronised clock on top of
// the vsync: on non-60Hz panels frames then straddle a varying number of
// composition ticks and dt oscillates, which reads as juddery motion at
// speed. Only pass a fixed target for loops that don't present every frame
// (or don't animate) — e.g. a logic-only tick loop with no canvas output,
// where nothing else throttles the loop and an uncapped run would peg a
// CPU core.
// QPC ticks per display refresh from the DWM compositor, 0 when
// unavailable. qpcRefreshPeriod shares QueryPerformanceCounter's time
// base, so it compares directly against QPC deltas.
function DisplayRefreshTicks: Int64;
var
  ti: DWM_TIMING_INFO;
begin
  Result := 0;
  FillChar(ti, SizeOf(ti), 0);
  ti.cbSize := SizeOf(ti);
  if Succeeded(DwmGetCompositionTimingInfo(0, ti)) then
    Result := Int64(ti.qpcRefreshPeriod);
end;

function runGameLoopNative(argCount: integer; args: pValue): TValue;
var
  targetFps, dt, remainMs: Double;
  freq, tPrev, tNow, frameTicks, deadline: Int64;
  rawTicks, refreshTicks, k: Int64;
  presentedThisFrame: Boolean;
  presentCountBefore: Int64;
  cb, dummy, dtArg: TValue;
begin
  Result := CreateNilValue;
  if ActiveEngine.Dispatching then
  begin
    RuntimeError('runGameLoop() cannot be called from inside an event callback.');
    Exit;
  end;
  if ActiveEngine.GameLoopActive then
  begin
    RuntimeError('runGameLoop() is already running.');
    Exit;
  end;
  targetFps := 0;  // uncapped by default — present() paces via vsync (DwmFlush)
  if argCount > 0 then
  begin
    if (argCount <> 1) or not isNumber(args[0]) then
    begin
      RuntimeError('runGameLoop() takes an optional target-FPS number argument.');
      Exit;
    end;
    targetFps := GetNumber(args[0]);
    if (targetFps < 0) or (targetFps > 1000) then
    begin
      RuntimeError('runGameLoop() target FPS must be between 0 (uncapped) and 1000.');
      Exit;
    end;
  end;

  QueryPerformanceFrequency(freq);
  if targetFps > 0 then
    frameTicks := Round(freq / targetFps)
  else
    frameTicks := 0;
  QueryPerformanceCounter(tPrev);
  deadline := tPrev + frameTicks;
  // No previous frame yet, so nothing to snap on the first iteration.
  presentedThisFrame := False;

  ActiveEngine.GameLoopActive := True;
  // Hold 1ms timer resolution for the duration of the loop (released in the
  // finally below) so the pacing Sleep(1) really is ~1ms.
  AcquireHighResTiming;
  try
    while ActiveEngine.GameLoopActive do
    begin
      ActiveEngine.FlushOutput;
      // Reset keyPressed()/mouseClicked() edge state before the pump so the
      // polling natives also work per-frame inside onFrame.
      ClearFrameEdges;
      Application.ProcessMessages;
      ActiveEngine.DispatchPendingEvents;
      if VM.RuntimeErrorStr <> '' then Exit;
      if not ActiveEngine.Running then
      begin
        RuntimeError('Script aborted: window closed.');
        Exit;
      end;
      if Assigned(ActiveEngine.OnCheckAbort) and ActiveEngine.OnCheckAbort() then
      begin
        RuntimeError('Script aborted: application closing.');
        Exit;
      end;
      // An event callback may have called stopGameLoop() during dispatch.
      if not ActiveEngine.GameLoopActive then Break;

      QueryPerformanceCounter(tNow);
      rawTicks := tNow - tPrev;
      tPrev := tNow;

      // De-jitter dt for vsynced scripts. present() blocks on DwmFlush,
      // so frames are DISPLAYED at exact multiples of the refresh
      // period — but the measured loop time also contains scheduler
      // wake-up noise, ProcessMessages bursts and DwmFlush return
      // jitter (±1-2ms). Feeding that noise into onFrame(dt) becomes
      // positional jitter proportional to movement speed. When the
      // measured interval is within a quarter period of a whole number
      // of refreshes, the true display interval WAS that whole number —
      // snap to it. Only apply this to frames whose previous iteration
      // actually presented (i.e. was paced by DwmFlush); a heavy
      // non-presenting frame that happens to land near a refresh
      // multiple must NOT be snapped or we'd inject phantom time into
      // its dt. Genuine hitches on presenting frames (> k ± period/4)
      // pass through unsnapped.
      refreshTicks := DisplayRefreshTicks;
      if presentedThisFrame and (refreshTicks > 0) then
      begin
        k := (rawTicks + refreshTicks div 2) div refreshTicks;  // nearest multiple
        if (k >= 1) and (Abs(rawTicks - k * refreshTicks) <= refreshTicks div 4) then
          rawTicks := k * refreshTicks;
      end;

      dt := rawTicks / freq;
      if dt > 0.1 then dt := 0.1;

      // Re-fetch each frame: the script may re-register onFrame mid-loop.
      cb := ActiveEngine.FrameCallback;
      if isClosure(cb) then
      begin
        // Snapshot the present count so we can tell whether onFrame
        // called present() this iteration; that gates the snap on the
        // NEXT iteration's rawTicks.
        presentCountBefore := CanvasPresentCount;
        dtArg := CreateNumber(dt);
        InvokeCallback(cb, [dtArg], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
        presentedThisFrame := CanvasPresentCount <> presentCountBefore;
      end
      else
        presentedThisFrame := False;
      if not ActiveEngine.GameLoopActive then Break;

      if frameTicks > 0 then
      begin
        QueryPerformanceCounter(tNow);
        if tNow >= deadline then
          deadline := tNow + frameTicks   // overran: restart pacing from now
        else
        begin
          while tNow < deadline do
          begin
            remainMs := (deadline - tNow) * 1000 / freq;
            if remainMs > 2 then
              TThread.Sleep(1)
            else
              TThread.Yield;
            QueryPerformanceCounter(tNow);
          end;
          deadline := deadline + frameTicks;
        end;
      end;
    end;
  finally
    ReleaseHighResTiming;
    ActiveEngine.GameLoopActive := False;
  end;
end;

// --- Simulation natives (for scripted testing) ---
// Each native feeds BOTH input paths a real device event would reach:
// the event engine's queues (callback dispatch — onKeyPressed etc.)
// and, via CanvasSimulate*, the canvas poll state (keyPressed/keyHeld/
// mouseX/mouseDown/mouseClicked and the widget engine). The canvas
// side is applied at the next frame-edge reset, so simulated input
// becomes visible to polls and widgets on the FRAME AFTER the
// simulate call — the same latency real pumped input has. Mouse
// coordinates are LOGICAL canvas pixels.

function simulateKeyDownNative(argCount: integer; args: pValue): TValue;
var
  key: string;
begin
  if (argCount <> 1) or not isString(args[0]) then
  begin
    RuntimeError('simulateKeyDown() takes 1 string argument.');
    Exit(CreateNilValue);
  end;
  key := string(AsAnsiString(args[0]));
  ActiveEngine.QueueKeyDown(key);
  CanvasSimulateKey(key, True);
  Result := CreateNilValue;
end;

function simulateKeyUpNative(argCount: integer; args: pValue): TValue;
var
  key: string;
begin
  if (argCount <> 1) or not isString(args[0]) then
  begin
    RuntimeError('simulateKeyUp() takes 1 string argument.');
    Exit(CreateNilValue);
  end;
  key := string(AsAnsiString(args[0]));
  ActiveEngine.QueueKeyUp(key);
  CanvasSimulateKey(key, False);
  Result := CreateNilValue;
end;

function simulateMouseDownNative(argCount: integer; args: pValue): TValue;
var
  btn, x, y: Integer;
begin
  if (argCount <> 3) or not isNumber(args[0]) or not isNumber(args[1]) or not isNumber(args[2]) then
  begin
    RuntimeError('simulateMouseDown() takes 3 number arguments (btn, x, y).');
    Exit(CreateNilValue);
  end;
  btn := Trunc(GetNumber(args[0]));
  x := Trunc(GetNumber(args[1]));
  y := Trunc(GetNumber(args[2]));
  ActiveEngine.QueueMouseDown(btn, x, y);
  CanvasSimulateMouseButton(btn, True, x, y);
  Result := CreateNilValue;
end;

function simulateMouseUpNative(argCount: integer; args: pValue): TValue;
var
  btn, x, y: Integer;
begin
  if (argCount <> 3) or not isNumber(args[0]) or not isNumber(args[1]) or not isNumber(args[2]) then
  begin
    RuntimeError('simulateMouseUp() takes 3 number arguments (btn, x, y).');
    Exit(CreateNilValue);
  end;
  btn := Trunc(GetNumber(args[0]));
  x := Trunc(GetNumber(args[1]));
  y := Trunc(GetNumber(args[2]));
  ActiveEngine.QueueMouseUp(btn, x, y);
  CanvasSimulateMouseButton(btn, False, x, y);
  Result := CreateNilValue;
end;

function simulateMouseMoveNative(argCount: integer; args: pValue): TValue;
var
  x, y: Integer;
begin
  if (argCount <> 2) or not isNumber(args[0]) or not isNumber(args[1]) then
  begin
    RuntimeError('simulateMouseMove() takes 2 number arguments (x, y).');
    Exit(CreateNilValue);
  end;
  x := Trunc(GetNumber(args[0]));
  y := Trunc(GetNumber(args[1]));
  ActiveEngine.QueueMouseMove(x, y);
  CanvasSimulateMouseMove(x, y);
  Result := CreateNilValue;
end;

// simulateChars(text) — inject text into the frame's WM_CHAR stream
// (what edit boxes read). Canvas-side only: there is no per-character
// event callback to queue. Use simulateKeyDown for keys that must also
// fire callbacks/nav edges; its char (if any) is synthesized
// automatically, so don't ALSO simulateChars the same keystroke.
function simulateCharsNative(argCount: integer; args: pValue): TValue;
begin
  if (argCount <> 1) or not isString(args[0]) then
  begin
    RuntimeError('simulateChars() takes 1 string argument.');
    Exit(CreateNilValue);
  end;
  CanvasSimulateChars(string(AsAnsiString(args[0])));
  Result := CreateNilValue;
end;

// --- Registration ---

procedure RegisterEventNatives;
begin
  // Callback natives: arity -1 = variadic (1 or 2 args accepted)
  defineNative('onKeyPressed', onKeyPressedNative, -1);
  defineNative('onKeyReleased', onKeyReleasedNative, -1);
  defineNative('onKeyHeld', onKeyHeldNative, 1);
  defineNative('onMouseDown', onMouseDownNative, -1);
  defineNative('onMouseUp', onMouseUpNative, -1);
  defineNative('onMouseMove', onMouseMoveNative, -1);
  defineNative('onClick', onClickNative, 2);
  defineNative('processEvents', processEventsNative, 0);
  // Engine-owned game loop
  defineNative('onFrame', onFrameNative, 1);
  defineNative('runGameLoop', runGameLoopNative, -1);  // 0-1 args
  defineNative('stopGameLoop', stopGameLoopNative, 0);
  // Simulation natives (for scripted testing — synthesize input
  // without VCL; feeds callbacks AND poll state AND widgets)
  defineNative('simulateKeyDown', simulateKeyDownNative, 1);
  defineNative('simulateKeyUp', simulateKeyUpNative, 1);
  defineNative('simulateMouseDown', simulateMouseDownNative, 3);
  defineNative('simulateMouseUp', simulateMouseUpNative, 3);
  defineNative('simulateMouseMove', simulateMouseMoveNative, 2);
  defineNative('simulateChars', simulateCharsNative, 1);
end;

end.
