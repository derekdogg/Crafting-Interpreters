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

  TLoxEventEngine = class
  private
    FCallbackSlots: TObjectDictionary<string, TCallbackSlot>;
    FMouseMoveX: Integer;
    FMouseMoveY: Integer;
    FMouseMoveSender: string;
    FMouseMoved: Boolean;
    FHeldKeys: TStringList;
    FPendingKeys: TStringList;
    FPendingMouse: TStringList;
    FPendingClicks: TStringList;
    FRunning: Boolean;
    FOnLog: TLogEvent;
    FOnCheckAbort: TCheckAbortFunc;
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
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnCheckAbort: TCheckAbortFunc read FOnCheckAbort write FOnCheckAbort;
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
  FPendingKeys := TStringList.Create;
  FPendingMouse := TStringList.Create;
  FPendingClicks := TStringList.Create;
  FHeldKeys := TStringList.Create;
  FHeldKeys.Sorted := True;
  FHeldKeys.Duplicates := dupIgnore;
  Reset;
end;

destructor TLoxEventEngine.Destroy;
begin
  FCallbackSlots.Free;
  FPendingKeys.Free;
  FPendingMouse.Free;
  FPendingClicks.Free;
  FHeldKeys.Free;
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
  FHeldKeys.Clear;
  FCallbackSlots.Clear;  // TObjectDictionary frees owned slots
  FMouseMoved := False;
  FMouseMoveX := 0;
  FMouseMoveY := 0;
  FMouseMoveSender := '';
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
  if idx >= 0 then
    FHeldKeys.Delete(idx);
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
begin
  // Coalesce: only keep the latest position per frame
  FMouseMoveX := x;
  FMouseMoveY := y;
  FMouseMoveSender := Sender;
  FMouseMoved := True;
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
begin
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

  // Dispatch mouse move (coalesced — one event per frame with latest position)
  if FMouseMoved then
  begin
    cb := GetCallback(eckMouseMove, FMouseMoveSender);
    FMouseMoved := False;
    if isClosure(cb) then
    begin
      args[0] := CreateNumber(FMouseMoveX);
      args[1] := CreateNumber(FMouseMoveY);
      InvokeCallback(cb, [args[0], args[1]], dummy);
      if VM.RuntimeErrorStr <> '' then Exit;
    end;
  end;

  // Dispatch held keys every frame (for continuous movement)
  cb := GetCallback(eckKeyHeld, '');
  if isClosure(cb) and (FHeldKeys.Count > 0) then
  begin
    snapshot := TStringList.Create;
    try
      snapshot.Assign(FHeldKeys);
      for i := 0 to snapshot.Count - 1 do
      begin
        args[0] := CreateStringValue(AnsiString(snapshot[i]));
        InvokeCallback(cb, [args[0]], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
      end;
    finally
      snapshot.Free;
    end;
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
  // Slots created during script execution self-register in SetCallback.
  // This method re-registers any that survived a previous run (unusual
  // but harmless to call — duplicates are cheap via the linear scan in
  // UnregisterGCRoot).
  for slot in FCallbackSlots.Values do
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
