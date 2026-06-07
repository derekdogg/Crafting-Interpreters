unit LoxEventEngine;

// ============================================================
// Reusable event-dispatching engine for Lox scripts.
// Decouples key/mouse callback handling from any specific form.
//
// Usage:
//   1. Create a TLoxEventEngine instance
//   2. After InitVM, call Engine.RegisterNatives and Engine.RegisterGCRoots
//   3. In VCL event handlers, call Engine.QueueKeyDown/Up, QueueMouseDown/Up
//   4. The processEvents native pumps messages and calls Engine.DispatchPendingEvents
//   5. After script ends, call Engine.UnregisterGCRoots
// ============================================================

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Chunk_Types;

type
  TLogEvent = procedure(const Msg: string) of object;
  TCheckAbortFunc = reference to function: Boolean;

  TEventCallbackKind = (
    eckKeyPressed,
    eckKeyReleased,
    eckKeyHeld,
    eckMouseDown,
    eckMouseUp
  );

  TLoxEventEngine = class
  private
    FOnKeyPressed: TValue;
    FOnKeyReleased: TValue;
    FOnKeyHeld: TValue;
    FOnMouseDown: TValue;
    FOnMouseUp: TValue;
    FHeldKeys: TStringList;
    FPendingKeys: TStringList;
    FPendingMouse: TStringList;
    FRunning: Boolean;
    FOnLog: TLogEvent;
    FOnCheckAbort: TCheckAbortFunc;
  public
    constructor Create;
    destructor Destroy; override;

    // Call before each script run to clear state
    procedure Reset;

    // Set a callback slot (called by natives)
    procedure SetCallback(Kind: TEventCallbackKind; const Value: TValue);

    // Flush VM.PrintBuilder via OnLog
    procedure FlushOutput;

    // Queue events from VCL handlers
    procedure QueueKeyDown(const KeyName: string);
    procedure QueueKeyUp(const KeyName: string);
    procedure QueueMouseDown(btn, x, y: Integer);
    procedure QueueMouseUp(btn, x, y: Integer);

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
  FPendingKeys := TStringList.Create;
  FPendingMouse := TStringList.Create;
  FHeldKeys := TStringList.Create;
  FHeldKeys.Sorted := True;
  FHeldKeys.Duplicates := dupIgnore;
  Reset;
end;

destructor TLoxEventEngine.Destroy;
begin
  FPendingKeys.Free;
  FPendingMouse.Free;
  FHeldKeys.Free;
  inherited;
end;

procedure TLoxEventEngine.Reset;
begin
  FPendingKeys.Clear;
  FPendingMouse.Clear;
  FHeldKeys.Clear;
  FOnKeyPressed := CreateNilValue;
  FOnKeyReleased := CreateNilValue;
  FOnKeyHeld := CreateNilValue;
  FOnMouseDown := CreateNilValue;
  FOnMouseUp := CreateNilValue;
  FRunning := True;
end;

procedure TLoxEventEngine.QueueKeyDown(const KeyName: string);
begin
  if KeyName = '' then Exit;
  // Only fire 'pressed' on genuine first down, not auto-repeat
  if FHeldKeys.IndexOf(KeyName) < 0 then
  begin
    FHeldKeys.Add(KeyName);
    FPendingKeys.Add('down:' + KeyName);
  end;
end;

procedure TLoxEventEngine.QueueKeyUp(const KeyName: string);
var
  idx: Integer;
begin
  if KeyName = '' then Exit;
  idx := FHeldKeys.IndexOf(KeyName);
  if idx >= 0 then
    FHeldKeys.Delete(idx);
  FPendingKeys.Add('up:' + KeyName);
end;

procedure TLoxEventEngine.QueueMouseDown(btn, x, y: Integer);
begin
  FPendingMouse.Add('down:' + IntToStr(btn) + ':' + IntToStr(x) + ':' + IntToStr(y));
end;

procedure TLoxEventEngine.QueueMouseUp(btn, x, y: Integer);
begin
  FPendingMouse.Add('up:' + IntToStr(btn) + ':' + IntToStr(x) + ':' + IntToStr(y));
end;

procedure TLoxEventEngine.DispatchPendingEvents;
var
  i: Integer;
  s, keyName: string;
  parts: TArray<string>;
  dummy: TValue;
  btn, x, y: Integer;
  args: array[0..2] of TValue;
  snapshot: TStringList;
begin
  // Dispatch key events
  for i := 0 to FPendingKeys.Count - 1 do
  begin
    s := FPendingKeys[i];
    if s.StartsWith('down:') then
    begin
      keyName := s.Substring(5);
      if isClosure(FOnKeyPressed) then
      begin
        args[0] := CreateStringValue(AnsiString(keyName));
        InvokeCallback(FOnKeyPressed, [args[0]], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
      end;
    end
    else if s.StartsWith('up:') then
    begin
      keyName := s.Substring(3);
      if isClosure(FOnKeyReleased) then
      begin
        args[0] := CreateStringValue(AnsiString(keyName));
        InvokeCallback(FOnKeyReleased, [args[0]], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
      end;
    end;
  end;
  FPendingKeys.Clear;

  // Dispatch mouse events
  for i := 0 to FPendingMouse.Count - 1 do
  begin
    s := FPendingMouse[i];
    parts := s.Split([':']);
    if Length(parts) = 4 then
    begin
      btn := StrToIntDef(parts[1], 0);
      x := StrToIntDef(parts[2], 0);
      y := StrToIntDef(parts[3], 0);
      args[0] := CreateNumber(btn);
      args[1] := CreateNumber(x);
      args[2] := CreateNumber(y);
      if (parts[0] = 'down') and isClosure(FOnMouseDown) then
      begin
        InvokeCallback(FOnMouseDown, [args[0], args[1], args[2]], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
      end
      else if (parts[0] = 'up') and isClosure(FOnMouseUp) then
      begin
        InvokeCallback(FOnMouseUp, [args[0], args[1], args[2]], dummy);
        if VM.RuntimeErrorStr <> '' then Exit;
      end;
    end;
  end;
  FPendingMouse.Clear;

  // Dispatch held keys every frame (for continuous movement)
  if isClosure(FOnKeyHeld) and (FHeldKeys.Count > 0) then
  begin
    snapshot := TStringList.Create;
    try
      snapshot.Assign(FHeldKeys);
      for i := 0 to snapshot.Count - 1 do
      begin
        args[0] := CreateStringValue(AnsiString(snapshot[i]));
        InvokeCallback(FOnKeyHeld, [args[0]], dummy);
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

procedure TLoxEventEngine.SetCallback(Kind: TEventCallbackKind; const Value: TValue);
begin
  case Kind of
    eckKeyPressed:  FOnKeyPressed := Value;
    eckKeyReleased: FOnKeyReleased := Value;
    eckKeyHeld:     FOnKeyHeld := Value;
    eckMouseDown:   FOnMouseDown := Value;
    eckMouseUp:     FOnMouseUp := Value;
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
begin
  Chunk_Types.RegisterGCRoot(FOnKeyPressed);
  Chunk_Types.RegisterGCRoot(FOnKeyReleased);
  Chunk_Types.RegisterGCRoot(FOnKeyHeld);
  Chunk_Types.RegisterGCRoot(FOnMouseDown);
  Chunk_Types.RegisterGCRoot(FOnMouseUp);
end;

procedure TLoxEventEngine.UnregisterGCRoots;
begin
  Chunk_Types.UnregisterGCRoot(FOnKeyPressed);
  Chunk_Types.UnregisterGCRoot(FOnKeyReleased);
  Chunk_Types.UnregisterGCRoot(FOnKeyHeld);
  Chunk_Types.UnregisterGCRoot(FOnMouseDown);
  Chunk_Types.UnregisterGCRoot(FOnMouseUp);
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
