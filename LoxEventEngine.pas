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
  System.Generics.Collections, Chunk_Types;

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
    eckClick
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

  TLoxEventEngine = class
  private
    FCallbackSlots: TObjectDictionary<string, TCallbackSlot>;
    FPendingMoves: TDictionary<string, TMouseMoveEntry>;
    FHeldKeys: TStringList;
    FPendingKeys: TStringList;
    FPendingMouse: TStringList;
    FPendingClicks: TStringList;
    FDispatching: Boolean;
    FRunning: Boolean;
    FOnLog: TLogEvent;
    FOnCheckAbort: TCheckAbortFunc;
    FHeldDispatchIntervalMs: DWORD;
    FHeldDispatchInitialDelayMs: DWORD;
    FHeldPressTicks: TDictionary<string, DWORD>;
    function CallbackKey(Kind: TEventCallbackKind; const ControlName: string): string;
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

    // Register the event natives into the current VM
    procedure RegisterNatives;

    // GC root management (call after InitVM / before FreeVM)
    procedure RegisterGCRoots;
    procedure UnregisterGCRoots;

    // Utility: convert a VK code to a friendly key name
    class function MapVKToName(Key: Word): string;

    property Running: Boolean read FRunning write FRunning;
    property Dispatching: Boolean read FDispatching;
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

implementation

uses
  EventNatives;

// --- TLoxEventEngine ---

constructor TLoxEventEngine.Create;
begin
  inherited Create;
  FCallbackSlots := TObjectDictionary<string, TCallbackSlot>.Create([doOwnsValues]);
  FPendingMoves := TDictionary<string, TMouseMoveEntry>.Create;
  FPendingKeys := TStringList.Create;
  FPendingMouse := TStringList.Create;
  FPendingClicks := TStringList.Create;
  FHeldKeys := TStringList.Create;
  FHeldKeys.Sorted := True;
  FHeldKeys.Duplicates := dupIgnore;
  FHeldPressTicks := TDictionary<string, DWORD>.Create;
  FHeldDispatchIntervalMs := 50;        // repeat ~20Hz once held
  FHeldDispatchInitialDelayMs := 250;   // a tap shorter than this fires no held events
  Reset;
end;

destructor TLoxEventEngine.Destroy;
begin
  FCallbackSlots.Free;
  FPendingMoves.Free;
  FPendingKeys.Free;
  FPendingMouse.Free;
  FPendingClicks.Free;
  FHeldKeys.Free;
  FHeldPressTicks.Free;
  inherited;
end;

function TLoxEventEngine.CallbackKey(Kind: TEventCallbackKind;
  const ControlName: string): string;
begin
  Result := IntToStr(Ord(Kind)) + ':' + LowerCase(ControlName);
end;

function TLoxEventEngine.GetCallback(Kind: TEventCallbackKind;
  const ControlName: string): TValue;
var
  slot: TCallbackSlot;
begin
  if FCallbackSlots.TryGetValue(CallbackKey(Kind, ControlName), slot) then
    Result := slot.Callback
  else
    Result := CreateNilValue;
end;

procedure TLoxEventEngine.Reset;
begin
  FPendingKeys.Clear;
  FPendingMouse.Clear;
  FPendingClicks.Clear;
  FPendingMoves.Clear;
  FHeldKeys.Clear;
  FHeldPressTicks.Clear;
  FCallbackSlots.Clear;  // TObjectDictionary frees owned slots
  FRunning := True;
end;

procedure TLoxEventEngine.QueueKeyDown(const KeyName: string;
  const Sender: string);
begin
  if KeyName = '' then Exit;
  // Only fire 'pressed' on genuine first down, not auto-repeat
  if FHeldKeys.IndexOf(KeyName) < 0 then
  begin
    FHeldKeys.Add(KeyName);
    // Mark the press time so onKeyHeld can apply an initial-repeat delay.
    FHeldPressTicks.AddOrSetValue(KeyName, GetTickCount);
    FPendingKeys.Add('down:' + Sender + ':' + KeyName);
  end;
end;

procedure TLoxEventEngine.QueueKeyUp(const KeyName: string;
  const Sender: string);
var
  idx: Integer;
begin
  if KeyName = '' then Exit;
  idx := FHeldKeys.IndexOf(KeyName);
  // Dedupe: only fire 'up' if we believe the key was held. Otherwise this is
  // a duplicate from a second source (e.g. form KeyPreview + canvas handler).
  if idx < 0 then Exit;
  FHeldKeys.Delete(idx);
  FHeldPressTicks.Remove(KeyName);
  FPendingKeys.Add('up:' + Sender + ':' + KeyName);
end;

procedure TLoxEventEngine.QueueMouseDown(btn, x, y: Integer;
  const Sender: string);
begin
  FPendingMouse.Add('down:' + Sender + ':' + IntToStr(btn) + ':' +
    IntToStr(x) + ':' + IntToStr(y));
end;

procedure TLoxEventEngine.QueueMouseUp(btn, x, y: Integer;
  const Sender: string);
begin
  FPendingMouse.Add('up:' + Sender + ':' + IntToStr(btn) + ':' +
    IntToStr(x) + ':' + IntToStr(y));
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
  FPendingClicks.Add(Sender);
end;

procedure TLoxEventEngine.DispatchPendingEvents;
var
  i: Integer;
  s, keyName, sender: string;
  parts: TArray<string>;
  dummy: TValue;
  cb: TValue;
  btn, x, y: Integer;
  args: array[0..2] of TValue;
  snapshot: TStringList;
  moveEntry: TMouseMoveEntry;
  pressTick, nowTick: DWORD;
begin
  FDispatching := True;
  try
  // Dispatch key events  (format: 'down:sender:keyname' / 'up:sender:keyname')
  for i := 0 to FPendingKeys.Count - 1 do
  begin
    s := FPendingKeys[i];
    parts := s.Split([':'], 3);  // at most 3 parts
    if Length(parts) = 3 then
    begin
      sender := parts[1];
      keyName := parts[2];
      if parts[0] = 'down' then
      begin
        cb := GetCallback(eckKeyPressed, sender);
        if isClosure(cb) then
        begin
          args[0] := CreateStringValue(AnsiString(keyName));
          InvokeCallback(cb, [args[0]], dummy);
          if VM.RuntimeErrorStr <> '' then Exit;
        end;
      end
      else if parts[0] = 'up' then
      begin
        cb := GetCallback(eckKeyReleased, sender);
        if isClosure(cb) then
        begin
          args[0] := CreateStringValue(AnsiString(keyName));
          InvokeCallback(cb, [args[0]], dummy);
          if VM.RuntimeErrorStr <> '' then Exit;
        end;
      end;
    end;
  end;
  FPendingKeys.Clear;

  // Dispatch mouse events  (format: 'down:sender:btn:x:y' / 'up:sender:btn:x:y')
  for i := 0 to FPendingMouse.Count - 1 do
  begin
    s := FPendingMouse[i];
    parts := s.Split([':']);
    if Length(parts) = 5 then
    begin
      sender := parts[1];
      btn := StrToIntDef(parts[2], 0);
      x := StrToIntDef(parts[3], 0);
      y := StrToIntDef(parts[4], 0);
      args[0] := CreateNumber(btn);
      args[1] := CreateNumber(x);
      args[2] := CreateNumber(y);
      if parts[0] = 'down' then
      begin
        cb := GetCallback(eckMouseDown, sender);
        if isClosure(cb) then
        begin
          InvokeCallback(cb, [args[0], args[1], args[2]], dummy);
          if VM.RuntimeErrorStr <> '' then Exit;
        end;
      end
      else if parts[0] = 'up' then
      begin
        cb := GetCallback(eckMouseUp, sender);
        if isClosure(cb) then
        begin
          InvokeCallback(cb, [args[0], args[1], args[2]], dummy);
          if VM.RuntimeErrorStr <> '' then Exit;
        end;
      end;
    end;
  end;
  FPendingMouse.Clear;

  // Dispatch click events  (each entry is a control name)
  for i := 0 to FPendingClicks.Count - 1 do
  begin
    sender := FPendingClicks[i];
    cb := GetCallback(eckClick, sender);
    if isClosure(cb) then
    begin
      InvokeCallback(cb, [], dummy);
      if VM.RuntimeErrorStr <> '' then Exit;
    end;
  end;
  FPendingClicks.Clear;

  // Dispatch mouse moves (coalesced per sender — one event per control per frame)
  for sender in FPendingMoves.Keys do
  begin
    cb := GetCallback(eckMouseMove, sender);
    if isClosure(cb) then
    begin
      moveEntry := FPendingMoves[sender];
      args[0] := CreateNumber(moveEntry.X);
      args[1] := CreateNumber(moveEntry.Y);
      InvokeCallback(cb, [args[0], args[1]], dummy);
      if VM.RuntimeErrorStr <> '' then Exit;
    end;
  end;
  FPendingMoves.Clear;

  // Dispatch held keys at a capped rate (for continuous movement).
  // Each key has its own timer so the initial delay (HeldDispatchInitialDelayMs)
  // prevents a quick tap from firing onKeyHeld at all, and subsequent fires are
  // spaced by HeldDispatchIntervalMs. Mirrors OS keyboard auto-repeat.
  cb := GetCallback(eckKeyHeld, '');
  if isClosure(cb) and (FHeldKeys.Count > 0) then
  begin
    snapshot := TStringList.Create;
    try
      snapshot.Assign(FHeldKeys);
      for i := 0 to snapshot.Count - 1 do
      begin
        keyName := snapshot[i];
        if not FHeldPressTicks.TryGetValue(keyName, pressTick) then
          Continue;
        nowTick := GetTickCount;
        if (nowTick - pressTick) < FHeldDispatchInitialDelayMs then
          Continue; // still inside the initial-press delay window
        args[0] := CreateStringValue(AnsiString(keyName));
        InvokeCallback(cb, [args[0]], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
        // Advance the per-key timer by one interval (not to "now") so we
        // catch up if processEvents was called less frequently than the rate.
        FHeldPressTicks[keyName] := pressTick + FHeldDispatchIntervalMs;
      end;
    finally
      snapshot.Free;
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
  key := CallbackKey(Kind, ControlName);
  if FCallbackSlots.TryGetValue(key, slot) then
  begin
    // Existing slot — just update the value (GC root address unchanged)
    slot.Callback := Value;
  end
  else
  begin
    // New slot — allocate and register as GC root
    slot := TCallbackSlot.Create;
    slot.Callback := Value;
    FCallbackSlots.Add(key, slot);
    Chunk_Types.RegisterGCRoot(slot.Callback);
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
  slot: TCallbackSlot;
begin
  // Only register slots that aren't already rooted. SetCallback self-registers
  // new slots, so calling this after callbacks exist must not create duplicates
  // (UnregisterGCRoot only removes the first match, leaving orphans behind).
  for slot in FCallbackSlots.Values do
    if not Chunk_Types.IsGCRootRegistered(slot.Callback) then
      Chunk_Types.RegisterGCRoot(slot.Callback);
end;

procedure TLoxEventEngine.UnregisterGCRoots;
var
  slot: TCallbackSlot;
begin
  for slot in FCallbackSlots.Values do
    Chunk_Types.UnregisterGCRoot(slot.Callback);
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
