unit LoxEventEngine;

// ============================================================
// Reusable event-dispatching engine for Lox scripts.
// Decouples key/mouse callback handling from any specific form.
//
// Usage:
//   1. Create a TLoxEventEngine instance
//   2. After InitVM, call Engine.RegisterNatives and Engine.RegisterGCRoots
//   3. In VCL event handlers, call Engine.QueueKeyDown/Up, QueueMouseDown/Up/Click
//   4. The processEvents native pumps messages and calls Engine.DispatchPendingEvents
//   5. After script ends, call Engine.UnregisterGCRoots
//
// Named-target callbacks
// ----------------------
// Each callback registration native (onMouseDown, onKeyPressed, etc.)
// accepts an optional first string argument naming the VCL control:
//   onMouseDown(fun(btn,x,y) { ... })            -- form/canvas default
//   onMouseDown("btnSave", fun(btn,x,y) { ... })  -- specific control
// The engine stores callbacks keyed by (Kind, ControlName).  An empty
// ControlName is the default/canvas-level slot.
// ============================================================

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  System.Generics.Collections, Suto;

type
  TLogEvent = procedure(const Msg: string) of object;
  TCheckAbortFunc = reference to function: Boolean;

  TEventCallbackKind = (
    eckKeyPressed,
    eckKeyReleased,
    eckKeyHeld,
    eckMouseDown,
    eckMouseUp,
    eckMouseMove,
    eckClick,
    eckFrame       // per-frame update callback used by runGameLoop()
  );

  // Heap-allocated slot so the TValue address is stable for GC roots.
  TCallbackSlot = class
  public
    Callback: TValue;
  end;

  // Per-sender coalesced mouse-move state.
  TMouseMoveEntry = record
    X, Y: Integer;
  end;

  // Typed pending key/mouse events. Queued and dispatched as records instead
  // of 'down:sender:key' style strings, to avoid paying a Split/StrToIntDef
  // parse (plus the string-building allocation on the Queue* side) for every
  // single event — this runs every frame in a game loop.
  TPendingKeyEvent = record
    IsDown: Boolean;
    Sender: string;
    KeyName: string;
  end;

  TPendingMouseEvent = record
    IsDown: Boolean;
    Sender: string;
    Btn, X, Y: Integer;
  end;

  TLoxEventEngine = class
  private
    // One dictionary per callback kind (keyed by already-lowercased control
    // name) instead of a single dictionary keyed by a concatenated
    // 'kind:controlname' string. The old scheme built (IntToStr + concat)
    // a fresh string key on every single lookup, which runs on every
    // dispatched event; this way the kind selects the dictionary directly
    // and the key is just the control name, normalized once at the point
    // callbacks/events are registered/queued rather than on every read.
    FCallbackSlots: array[TEventCallbackKind] of TObjectDictionary<string, TCallbackSlot>;
    FPendingMoves: TDictionary<string, TMouseMoveEntry>;
    FHeldKeys: TStringList;
    FPendingKeys: TList<TPendingKeyEvent>;
    FPendingMouse: TList<TPendingMouseEvent>;
    FPendingClicks: TStringList;
    FDispatching: Boolean;
    FRunning: Boolean;
    FGameLoopActive: Boolean;
    FOnLog: TLogEvent;
    FOnCheckAbort: TCheckAbortFunc;
    FHeldDispatchIntervalMs: DWORD;
    FHeldDispatchInitialDelayMs: DWORD;
    // Per-key tick (GetTickCount domain) at which onKeyHeld becomes eligible
    // to fire next. Seeded on key-down to (now + initial delay) so the initial
    // delay is a one-time gate; advanced by FHeldDispatchIntervalMs after each
    // dispatch so subsequent fires are spaced by the interval only.
    FHeldNextDispatchTick: TDictionary<string, DWORD>;
    function GetCallback(Kind: TEventCallbackKind; const ControlName: string): TValue;
  public
    constructor Create;
    destructor Destroy; override;

    // Call before each script run to clear state
    procedure Reset;

    // Set a callback slot (called by natives).  ControlName='' is default.
    procedure SetCallback(Kind: TEventCallbackKind; const ControlName: string;
      const Value: TValue);

    // Flush VM.PrintBuilder via OnLog
    procedure FlushOutput;

    // Queue events from VCL handlers.  Sender defaults to '' (canvas).
    procedure QueueKeyDown(const KeyName: string; const Sender: string = '');
    procedure QueueKeyUp(const KeyName: string; const Sender: string = '');
    procedure QueueMouseDown(btn, x, y: Integer; const Sender: string = '');
    procedure QueueMouseUp(btn, x, y: Integer; const Sender: string = '');
    procedure QueueMouseMove(x, y: Integer; const Sender: string = '');
    procedure QueueClick(const Sender: string);

    // Dispatch all pending events via InvokeCallback
    procedure DispatchPendingEvents;

    // The onFrame callback (nil TValue if none registered)
    function FrameCallback: TValue;

    // Register the event natives into the current VM
    procedure RegisterNatives;

    // GC root management (call after InitVM / before FreeVM)
    procedure RegisterGCRoots;
    procedure UnregisterGCRoots;

    // Utility: convert a VK code to a friendly key name
    class function MapVKToName(Key: Word): string;

    property Running: Boolean read FRunning write FRunning;
    property Dispatching: Boolean read FDispatching;
    // True while runGameLoop() is executing. stopGameLoop() clears it to
    // make the loop exit after the current frame.
    property GameLoopActive: Boolean read FGameLoopActive write FGameLoopActive;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnCheckAbort: TCheckAbortFunc read FOnCheckAbort write FOnCheckAbort;
    // Minimum gap (ms) between repeat onKeyHeld dispatches for a held key.
    property HeldDispatchIntervalMs: DWORD read FHeldDispatchIntervalMs write FHeldDispatchIntervalMs;
    // Delay (ms) after KeyDown before the first onKeyHeld fires for that key.
    // Set high enough that a quick tap fires zero held events.
    property HeldDispatchInitialDelayMs: DWORD read FHeldDispatchInitialDelayMs write FHeldDispatchInitialDelayMs;
  end;

var
  ActiveEngine: TLoxEventEngine;

// High-resolution timing, scoped and reference-counted. TThread.Sleep(1) only
// sleeps ~1ms while the process holds a 1ms timer-resolution request; at the
// default ~15.6ms resolution it quantizes frame loops to an uneven ~64Hz that
// judders against the monitor refresh. Holding the request process-wide for
// the engine's whole lifetime would raise CPU wakeups/power draw even while
// idle, so callers acquire it only around code that actually sleeps (the
// runGameLoop body, the Sleep inside processEvents) and release immediately
// after. Calls must be balanced; main-thread only.
procedure AcquireHighResTiming;
procedure ReleaseHighResTiming;

implementation

uses
  Winapi.MMSystem, EventNatives;

var
  HighResTimingRefs: Integer = 0;

procedure AcquireHighResTiming;
begin
  Inc(HighResTimingRefs);
  if HighResTimingRefs = 1 then
    timeBeginPeriod(1);
end;

procedure ReleaseHighResTiming;
begin
  if HighResTimingRefs <= 0 then Exit;  // tolerate unbalanced release
  Dec(HighResTimingRefs);
  if HighResTimingRefs = 0 then
    timeEndPeriod(1);
end;

// --- TLoxEventEngine ---

constructor TLoxEventEngine.Create;
var
  kind: TEventCallbackKind;
begin
  inherited Create;
  for kind := Low(TEventCallbackKind) to High(TEventCallbackKind) do
    FCallbackSlots[kind] := TObjectDictionary<string, TCallbackSlot>.Create([doOwnsValues]);
  FPendingMoves := TDictionary<string, TMouseMoveEntry>.Create;
  FPendingKeys := TList<TPendingKeyEvent>.Create;
  FPendingMouse := TList<TPendingMouseEvent>.Create;
  FPendingClicks := TStringList.Create;
  FHeldKeys := TStringList.Create;
  FHeldKeys.Sorted := True;
  FHeldKeys.Duplicates := dupIgnore;
  FHeldNextDispatchTick := TDictionary<string, DWORD>.Create;
  // Default: fire onKeyHeld every processEvents() with no initial delay.
  // This matches the legacy behavior the existing test suite expects.
  // Hosts that want OS-style auto-repeat (a tap fires zero held events,
  // a hold streams them) should set HeldDispatchInitialDelayMs and
  // HeldDispatchIntervalMs themselves (e.g. 250 / 50).
  FHeldDispatchIntervalMs := 0;
  FHeldDispatchInitialDelayMs := 0;
  Reset;
end;

destructor TLoxEventEngine.Destroy;
var
  kind: TEventCallbackKind;
begin
  for kind := Low(TEventCallbackKind) to High(TEventCallbackKind) do
    FCallbackSlots[kind].Free;
  FPendingMoves.Free;
  FPendingKeys.Free;
  FPendingMouse.Free;
  FPendingClicks.Free;
  FHeldKeys.Free;
  FHeldNextDispatchTick.Free;
  inherited;
end;

function TLoxEventEngine.GetCallback(Kind: TEventCallbackKind;
  const ControlName: string): TValue;
var
  slot: TCallbackSlot;
begin
  // ControlName is expected already-canonical (lowercased at the Queue*/
  // SetCallback boundary) so no LowerCase() call is needed on this hot path.
  if FCallbackSlots[Kind].TryGetValue(ControlName, slot) then
    Result := slot.Callback
  else
    Result := CreateNilValue;
end;

procedure TLoxEventEngine.Reset;
var
  kind: TEventCallbackKind;
begin
  // Safe to destroy callback slots directly here rather than requiring a
  // prior UnregisterGCRoots call: Reset() is invoked at the START of a run,
  // BEFORE InitVM creates the new VM instance whose ExtraRoots array
  // RegisterGCRoots will populate. Any roots from a PREVIOUS run were
  // already removed by that run's UnregisterGCRoots (called in its
  // `finally`, before FreeVM) — see TfrmGame.RunScript for the canonical
  // order: Reset -> InitVM -> RegisterGCRoots -> run -> UnregisterGCRoots
  // -> FreeVM. So there is never a live VM holding a pointer into a slot
  // this Reset() is about to free.
  FPendingKeys.Clear;
  FPendingMouse.Clear;
  FPendingClicks.Clear;
  FPendingMoves.Clear;
  FHeldKeys.Clear;
  FHeldNextDispatchTick.Clear;
  for kind := Low(TEventCallbackKind) to High(TEventCallbackKind) do
    FCallbackSlots[kind].Clear;  // TObjectDictionary frees owned slots
  FRunning := True;
  FGameLoopActive := False;
end;

function TLoxEventEngine.FrameCallback: TValue;
begin
  Result := GetCallback(eckFrame, '');
end;

procedure TLoxEventEngine.QueueKeyDown(const KeyName: string;
  const Sender: string);
var
  evt: TPendingKeyEvent;
begin
  if KeyName = '' then Exit;
  // Only fire 'pressed' on genuine first down, not auto-repeat
  if FHeldKeys.IndexOf(KeyName) < 0 then
  begin
    FHeldKeys.Add(KeyName);
    // Seed the per-key timer to the first eligible held-dispatch tick:
    // (now + initial delay). DispatchPendingEvents compares against this
    // directly, so the initial delay is a one-time gate rather than being
    // re-applied after every held dispatch.
    FHeldNextDispatchTick.AddOrSetValue(KeyName,
      GetTickCount + FHeldDispatchInitialDelayMs);
    evt.IsDown := True;
    evt.Sender := LowerCase(Sender);
    evt.KeyName := KeyName;
    FPendingKeys.Add(evt);
  end;
end;

procedure TLoxEventEngine.QueueKeyUp(const KeyName: string;
  const Sender: string);
var
  idx: Integer;
  evt: TPendingKeyEvent;
begin
  if KeyName = '' then Exit;
  idx := FHeldKeys.IndexOf(KeyName);
  if idx >= 0 then
  begin
    FHeldKeys.Delete(idx);
    FHeldNextDispatchTick.Remove(KeyName);
  end;
  evt.IsDown := False;
  evt.Sender := LowerCase(Sender);
  evt.KeyName := KeyName;
  FPendingKeys.Add(evt);
end;

procedure TLoxEventEngine.QueueMouseDown(btn, x, y: Integer;
  const Sender: string);
var
  evt: TPendingMouseEvent;
begin
  evt.IsDown := True;
  evt.Sender := LowerCase(Sender);
  evt.Btn := btn;
  evt.X := x;
  evt.Y := y;
  FPendingMouse.Add(evt);
end;

procedure TLoxEventEngine.QueueMouseUp(btn, x, y: Integer;
  const Sender: string);
var
  evt: TPendingMouseEvent;
begin
  evt.IsDown := False;
  evt.Sender := LowerCase(Sender);
  evt.Btn := btn;
  evt.X := x;
  evt.Y := y;
  FPendingMouse.Add(evt);
end;

procedure TLoxEventEngine.QueueMouseMove(x, y: Integer;
  const Sender: string);
var
  entry: TMouseMoveEntry;
begin
  // Coalesce per sender: only keep the latest position per frame per control
  entry.X := x;
  entry.Y := y;
  FPendingMoves.AddOrSetValue(LowerCase(Sender), entry);
end;

procedure TLoxEventEngine.QueueClick(const Sender: string);
begin
  // Canonical lowercase, same as every other Queue* sender — GetCallback no
  // longer normalizes case itself, so all queued/registered names must land
  // here already-lowercased.
  FPendingClicks.Add(LowerCase(Sender));
end;

procedure TLoxEventEngine.DispatchPendingEvents;
var
  i: Integer;
  keyName, sender: string;
  dummy: TValue;
  cb: TValue;
  args: array[0..2] of TValue;
  keyEvents: TArray<TPendingKeyEvent>;
  mouseEvents: TArray<TPendingMouseEvent>;
  moveEvents: TArray<TPair<string, TMouseMoveEntry>>;
  clickSenders: TArray<string>;
  heldSnapshot: TArray<string>;
  pressTick, nowTick: DWORD;
begin
  FDispatching := True;
  try
  // Dispatch key events (typed records — no Split/StrToIntDef parsing).
  // ToArray + Clear BEFORE invoking any callback: a runtime error partway
  // through must not leave already-queued events to replay next frame, and a
  // callback that pumps messages (reentrant QueueKeyDown/Up) must land in the
  // now-empty live queue rather than the one we're currently reading.
  if FPendingKeys.Count > 0 then
  begin
    keyEvents := FPendingKeys.ToArray;
    FPendingKeys.Clear;
    for i := 0 to High(keyEvents) do
    begin
      if keyEvents[i].IsDown then
        cb := GetCallback(eckKeyPressed, keyEvents[i].Sender)
      else
        cb := GetCallback(eckKeyReleased, keyEvents[i].Sender);
      if isClosure(cb) then
      begin
        args[0] := CreateStringValue(AnsiString(keyEvents[i].KeyName));
        InvokeCallback(cb, [args[0]], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
      end;
    end;
  end;

  // Dispatch mouse events (typed records). Same ToArray-then-Clear-before-
  // dispatch ordering as keys, above, and for the same reason.
  if FPendingMouse.Count > 0 then
  begin
    mouseEvents := FPendingMouse.ToArray;
    FPendingMouse.Clear;
    for i := 0 to High(mouseEvents) do
    begin
      args[0] := CreateNumber(mouseEvents[i].Btn);
      args[1] := CreateNumber(mouseEvents[i].X);
      args[2] := CreateNumber(mouseEvents[i].Y);
      if mouseEvents[i].IsDown then
        cb := GetCallback(eckMouseDown, mouseEvents[i].Sender)
      else
        cb := GetCallback(eckMouseUp, mouseEvents[i].Sender);
      if isClosure(cb) then
      begin
        InvokeCallback(cb, [args[0], args[1], args[2]], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
      end;
    end;
  end;

  // Dispatch click events (each entry is a control name). Copy + clear
  // before dispatch, same reasoning as keys/mouse above. Plain array copy
  // instead of a TStringList.Create/Assign/Free round trip every frame.
  if FPendingClicks.Count > 0 then
  begin
    SetLength(clickSenders, FPendingClicks.Count);
    for i := 0 to FPendingClicks.Count - 1 do
      clickSenders[i] := FPendingClicks[i];
    FPendingClicks.Clear;
    for i := 0 to High(clickSenders) do
    begin
      sender := clickSenders[i];
      cb := GetCallback(eckClick, sender);
      if isClosure(cb) then
      begin
        InvokeCallback(cb, [], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
      end;
    end;
  end;

  // Dispatch mouse moves (coalesced per sender — one event per control per
  // frame). ToArray + Clear BEFORE invoking any callback: a Lox print inside
  // a callback pumps Windows messages (via VM.OnPrint), which can queue fresh
  // mouse moves — those must land in the now-empty live dictionary, not get
  // lost or double-processed against a dictionary we're mid-iteration on.
  if FPendingMoves.Count > 0 then
  begin
    moveEvents := FPendingMoves.ToArray;
    FPendingMoves.Clear;
    for i := 0 to High(moveEvents) do
    begin
      cb := GetCallback(eckMouseMove, moveEvents[i].Key);
      if not isClosure(cb) then Continue;
      args[0] := CreateNumber(moveEvents[i].Value.X);
      args[1] := CreateNumber(moveEvents[i].Value.Y);
      InvokeCallback(cb, [args[0], args[1]], dummy);
      if VM.RuntimeErrorStr <> '' then Exit;
    end;
  end;

  // Dispatch held keys at a capped rate (for continuous movement).
  // Each key has its own timer so the initial delay (HeldDispatchInitialDelayMs)
  // prevents a quick tap from firing onKeyHeld at all, and subsequent fires are
  // spaced by HeldDispatchIntervalMs. Mirrors OS keyboard auto-repeat.
  cb := GetCallback(eckKeyHeld, '');
  if isClosure(cb) and (FHeldKeys.Count > 0) then
  begin
    SetLength(heldSnapshot, FHeldKeys.Count);
    for i := 0 to FHeldKeys.Count - 1 do
      heldSnapshot[i] := FHeldKeys[i];
    nowTick := GetTickCount;  // one call per dispatch cycle, not per key
    for i := 0 to High(heldSnapshot) do
    begin
      keyName := heldSnapshot[i];
      if not FHeldNextDispatchTick.TryGetValue(keyName, pressTick) then
        Continue;
      // Tick-wrap-safe comparison: signed delta < 0 means not yet eligible.
      if Integer(nowTick - pressTick) < 0 then
        Continue; // still inside the initial-press delay window
      args[0] := CreateStringValue(AnsiString(keyName));
      InvokeCallback(cb, [args[0]], dummy);
      if VM.RuntimeErrorStr <> '' then Exit;
      // The callback may have pumped Windows messages (e.g. via a Lox print
      // hooked to a UI memo) and delivered a WM_KEYUP, which removes the
      // entry from FHeldNextDispatchTick. Only re-arm if the key is still
      // held; use AddOrSetValue rather than the indexer setter, which would
      // raise EListError on a missing key.
      if FHeldKeys.IndexOf(keyName) >= 0 then
        FHeldNextDispatchTick.AddOrSetValue(keyName,
          pressTick + FHeldDispatchIntervalMs);
    end;
  end;
  finally
    FDispatching := False;
  end;
end;

procedure TLoxEventEngine.RegisterNatives;
begin
  ActiveEngine := Self;
  EventNatives.RegisterEventNatives;
end;

procedure TLoxEventEngine.SetCallback(Kind: TEventCallbackKind;
  const ControlName: string; const Value: TValue);
var
  key: string;
  slot: TCallbackSlot;
begin
  key := LowerCase(ControlName);
  if FCallbackSlots[Kind].TryGetValue(key, slot) then
  begin
    // Existing slot — just update the value (GC root address unchanged).
    // No write barrier needed: Suto's GC roots (VM.ExtraRoots) store the
    // ADDRESS of this TValue field, not a snapshot of its contents, so a
    // plain assignment through that same memory is automatically visible
    // to the next mark pass — there's nothing to notify.
    slot.Callback := Value;
  end
  else
  begin
    // New slot — allocate and register as GC root
    slot := TCallbackSlot.Create;
    slot.Callback := Value;
    FCallbackSlots[Kind].Add(key, slot);
    Suto.RegisterGCRoot(slot.Callback);
  end;
end;

procedure TLoxEventEngine.FlushOutput;
begin
  if Assigned(FOnLog) and (VM.PrintBuilder.Length > 0) then
  begin
    FOnLog(VM.PrintBuilder.ToString);
    VM.PrintBuilder.Clear;
  end;
end;

procedure TLoxEventEngine.RegisterGCRoots;
var
  kind: TEventCallbackKind;
  slot: TCallbackSlot;
begin
  // Only register slots that aren't already rooted. SetCallback self-registers
  // new slots, so calling this after callbacks exist must not create duplicates
  // (UnregisterGCRoot only removes the first match, leaving orphans behind).
  for kind := Low(TEventCallbackKind) to High(TEventCallbackKind) do
    for slot in FCallbackSlots[kind].Values do
      if not Suto.IsGCRootRegistered(slot.Callback) then
        Suto.RegisterGCRoot(slot.Callback);
end;

procedure TLoxEventEngine.UnregisterGCRoots;
var
  kind: TEventCallbackKind;
  slot: TCallbackSlot;
begin
  for kind := Low(TEventCallbackKind) to High(TEventCallbackKind) do
    for slot in FCallbackSlots[kind].Values do
      Suto.UnregisterGCRoot(slot.Callback);
end;

class function TLoxEventEngine.MapVKToName(Key: Word): string;
begin
  case Key of
    VK_LEFT:   Result := 'left';
    VK_RIGHT:  Result := 'right';
    VK_UP:     Result := 'up';
    VK_DOWN:   Result := 'down';
    VK_SPACE:  Result := 'space';
    VK_RETURN: Result := 'enter';
    VK_ESCAPE: Result := 'escape';
    VK_BACK:   Result := 'backspace';
    VK_TAB:    Result := 'tab';
    VK_DELETE: Result := 'delete';
    VK_HOME:   Result := 'home';
    VK_END:    Result := 'end';
  else
    if (Key >= Ord('A')) and (Key <= Ord('Z')) then
      Result := Chr(Key + 32)  // lowercase
    else if (Key >= Ord('0')) and (Key <= Ord('9')) then
      Result := Chr(Key)
    else
      Result := '';
  end;
end;

end.
