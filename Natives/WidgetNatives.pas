unit WidgetNatives;

// ============================================================
// WidgetNatives — Lox surface for the LoxWidgets retained-mode
// widget engine.
//
// Registered functions (see LoxWidgets.pas for the full behaviour spec):
//
//   createButton(x, y, w, h, caption [, onClick]) -> number (widget id)
//     Adds a Delphi-styled push button at the given LOGICAL canvas
//     coordinates. Returns an integer id used by every other native
//     in this module. The optional onClick function is attached as
//     the button's click handler (see setButtonOnClick).
//
//   createEdit(x, y, w, h [, text]) -> number (widget id)
//     Adds a classic single-line edit box. Click focuses it and
//     places the caret; typing/backspace autorepeat like a real
//     TEdit; left/right/home/end/delete navigate (edge-triggered).
//     Enter fires the submitted edge.
//
//   createCombo(x, y, w, h) -> number (widget id)
//     Adds a drop-down combo (pick-from-list, no free text). Click
//     toggles the dropdown; it overlays other widgets and swallows
//     the pointer while open (click outside folds it). Up/down step
//     the selection when closed and the highlight when open; Enter
//     commits, Escape folds. Populate with addComboItem.
//
//   addComboItem(id, text)                 -> number (new item index)
//   clearComboItems(id)                    -> nil
//   comboItemCount(id)                     -> number
//   getComboIndex(id) / setComboIndex(id, i) -> number / nil
//   getComboText(id)                       -> string ('' if none)
//     Item-list accessors; 0-based indices, -1 = no selection.
//     setComboIndex does NOT fire the changed edge (same contract as
//     setEditText). Wrong-kind ids raise a runtime error; stale ids
//     are silently ignored.
//
//   comboChanged(id)                       -> bool   (consume-once)
//     Selection changed by the user (click or arrow keys).
//
//   createRadioGroup(x, y, w, h, caption) -> number (widget id)
//     Adds a radio-button group — ONE widget for the whole group
//     (TRadioGroup-style), so mutual exclusivity is just its item
//     index. Click an item to select; up/down move the selection
//     while focused. Items that don't fit the height are hidden
//     (size the group to its items).
//
//   addRadioItem / clearRadioItems / radioItemCount
//   getRadioIndex / setRadioIndex / getRadioText
//   radioChanged(id)                       -> (consume-once)
//     Radio counterparts of the combo item accessors, identical
//     contracts (0-based, -1 = none, programmatic set fires no edge).
//     setWidgetCaption changes the group's frame caption.
//
//   setButtonOnClick(id, fn)               -> nil
//   setEditOnChange(id, fn)                -> nil
//   setEditOnSubmit(id, fn)                -> nil
//   setComboOnChange(id, fn)               -> nil
//   setRadioOnChange(id, fn)               -> nil
//     Attach or replace a handler for one of the widget's edges; pass
//     nil to detach. Handlers fire from inside updateWidgets() when
//     their edge completes, and may declare zero parameters
//     (fun() {...}) or one (fun(id) {...}) — the id is passed when
//     declared, the Delphi Sender analogue, so one handler can serve
//     several widgets. A widget with a handler has that edge CONSUMED
//     by the dispatch, so the matching polling native (buttonClicked /
//     editChanged / editSubmitted) will not also report it; widgets
//     without a handler keep the polling behaviour unchanged.
//
//   getEditText(id)                        -> string
//   setEditText(id, text)                  -> nil
//   setEditMaxLength(id, n)                -> nil   (0 = unlimited)
//     Text accessors. setEditText moves the caret to the end and does
//     NOT fire the changed edge (scripts shouldn't observe their own
//     set). Using an existing non-edit widget id raises a runtime
//     error; stale ids are silently ignored ('' for the getter).
//
//   editChanged(id)                        -> bool   (consume-once)
//   editSubmitted(id)                      -> bool   (consume-once)
//     Polling counterparts of onChange / onSubmit.
//
//   focusWidget(idOrNil)                   -> nil
//   focusedWidget()                        -> number (0 = none)
//     Keyboard focus. Only the focused widget receives typed keys; a
//     click on empty canvas or a non-focusable widget clears focus.
//     Scripts mixing gameplay keys with widgets should gate their
//     keyPressed()/keyHeld() polling on focusedWidget() == 0 — the
//     engine does not steal keys from script polling.
//
//   Generic widget operations — canonical names; they work on ANY
//   widget kind, so new kinds (edit, combo, radio group, ...) get
//   them for free:
//
//   setWidgetCaption(id, caption)          -> nil
//   setWidgetBounds(id, x, y, w, h)        -> nil
//   setWidgetEnabled(id, enabled)          -> nil
//   setWidgetVisible(id, visible)          -> nil
//   freeWidget(id)                         -> nil
//   widgetEnabled(id)                      -> bool
//   widgetVisible(id)                      -> bool
//   clearWidgets()                         -> nil
//   widgetCount()                          -> number
//   widgetExists(id)                       -> bool
//
//   Compatibility aliases (same natives under the pre-rename button-
//   specific names; error messages report the canonical name):
//   setButtonCaption, setButtonBounds, setButtonEnabled,
//   setButtonVisible, freeButton, buttonEnabled, buttonVisible.
//
//   updateWidgets()                        -> nil
//     Refresh hover/pressed/click state for every widget from the
//     latest mouse position + button state. Call once per frame,
//     AFTER the message pump has run (runGameLoop pumps before every
//     onFrame call; in a manual loop that means after processMessages())
//     and BEFORE any polling of buttonClicked / hover / etc.
//
//   drawWidgets()                          -> nil
//     Render every visible widget over the current canvas back buffer.
//     Call between your world draw and present().
//
//   buttonClicked(id)                      -> bool
//     Consume-once. True the first time you call it after a full
//     press+release-inside gesture; False afterward until the next
//     click. This mirrors the polling pattern of keyPressed() /
//     mouseClicked() in the canvas natives.
//
//   buttonHover(id)                        -> bool
//   buttonPressed(id)                      -> bool
//     Non-consuming state peeks. Return False for unknown ids.
//
// Typical script skeleton (engine-owned loop — preferred; runGameLoop
// pumps messages and paces the frame rate for you):
//
//   var ok = createButton(60, 100, 80, 28, "OK");
//   var cancel = createButton(160, 100, 80, 28, "Cancel");
//   onFrame(fun(dt) {
//     updateWidgets();
//     if (buttonClicked(ok))     { print "OK"; stopGameLoop(); }
//     if (buttonClicked(cancel)) { print "no"; stopGameLoop(); }
//     clearCanvas();
//     drawText(60, 60, "Choose:");
//     drawWidgets();
//     present();
//   });
//   runGameLoop(60);
//
// A manual while-loop also works — call processMessages() (or
// processEvents()) yourself at the top of each iteration, before
// updateWidgets() — but it runs unpaced and needs a host that
// registers processMessages (fmGame does; fmEventTest does not).
// See samples/demos/widgets_buttons.lox for the full demo.
// ============================================================

interface

// Registers the widget natives and clears any state from a previous
// script run. Call once per VM lifecycle, right after
// RegisterCanvasNatives (the widget engine draws to the canvas and
// pulls its mouse state from LoxCanvas, so the canvas natives must
// exist for widgets to be usable).
procedure RegisterWidgetNatives;

implementation

uses
  System.SysUtils, System.Generics.Collections, Suto, LoxWidgets,
  NativeRegistry;

// -- Widget-callback storage ------------------------------------------
// Handlers live HERE, not in LoxWidgets: anything holding a TValue is
// a VM concern, and the widget engine stays VM-agnostic. Same slot
// pattern as TLoxEventEngine — a heap-allocated holder gives each
// TValue a stable address for RegisterGCRoot.

type
  // One edge kind per consume-once edge the engine exposes.
  TWidgetCallbackKind = (wcbClick, wcbChange, wcbSubmit);

  TCallbackSlot = class
  public
    Callbacks: array[TWidgetCallbackKind] of TValue;
    constructor Create;
  end;

constructor TCallbackSlot.Create;
var
  k: TWidgetCallbackKind;
begin
  inherited Create;
  for k := Low(TWidgetCallbackKind) to High(TWidgetCallbackKind) do
    Callbacks[k] := CreateNilValue;
end;

var
  // Widget id -> handler slot (all edge kinds for that widget).
  FCallbackSlots: TObjectDictionary<Integer, TCallbackSlot> = nil;
  // True while updateWidgets() is dispatching handlers; guards against
  // a handler calling updateWidgets() re-entrantly.
  FDispatchingCallbacks: Boolean = False;

// Drop every stored handler. When the current VM still owns the slots
// (clearWidgets mid-run) the GC roots must be unregistered before the
// slot objects are freed, or the collector would scan freed memory.
// At RegisterWidgetNatives time the roots belonged to the previous,
// already-freed VM (roots live in VM.ExtraRoots), so UnregisterGCRoot
// finds no match and is a harmless no-op.
procedure ClearCallbackSlots;
var
  slot: TCallbackSlot;
  k: TWidgetCallbackKind;
begin
  if FCallbackSlots = nil then Exit;
  for slot in FCallbackSlots.Values do
    for k := Low(TWidgetCallbackKind) to High(TWidgetCallbackKind) do
      UnregisterGCRoot(slot.Callbacks[k]);
  FCallbackSlots.Clear;  // doOwnsValues frees the slot objects
end;

procedure RemoveCallbackSlot(id: Integer);
var
  slot: TCallbackSlot;
  k: TWidgetCallbackKind;
begin
  if (FCallbackSlots <> nil) and FCallbackSlots.TryGetValue(id, slot) then
  begin
    for k := Low(TWidgetCallbackKind) to High(TWidgetCallbackKind) do
      UnregisterGCRoot(slot.Callbacks[k]);
    FCallbackSlots.Remove(id);  // frees the slot (doOwnsValues)
  end;
end;

// Attach (or with a nil TValue: detach) one handler. Creates the slot
// on first use and roots all of its TValue cells once — later
// assignments reuse the same addresses, so roots never duplicate.
procedure SetWidgetCallback(id: Integer; kind: TWidgetCallbackKind;
  const cb: TValue);
var
  slot: TCallbackSlot;
  k: TWidgetCallbackKind;
begin
  if FCallbackSlots = nil then
    FCallbackSlots := TObjectDictionary<Integer, TCallbackSlot>.Create([doOwnsValues]);
  if not FCallbackSlots.TryGetValue(id, slot) then
  begin
    slot := TCallbackSlot.Create;
    FCallbackSlots.Add(id, slot);
    for k := Low(TWidgetCallbackKind) to High(TWidgetCallbackKind) do
      RegisterGCRoot(slot.Callbacks[k]);
  end;
  slot.Callbacks[kind] := cb;
end;

// Detach one handler without creating a slot for widgets that never
// had one.
procedure DetachWidgetCallback(id: Integer; kind: TWidgetCallbackKind);
var
  slot: TCallbackSlot;
begin
  if (FCallbackSlots <> nil) and FCallbackSlots.TryGetValue(id, slot) then
    slot.Callbacks[kind] := CreateNilValue;
end;

// -- Small helpers to keep native bodies short. All native funcs share
//    a common shape: validate arg count, coerce to numbers/strings,
//    delegate, return TValue. These helpers own the coercion + error
//    handling so the delegation stays a one-liner.

function ArgNumOrErr(args: pValue; idx: Integer; const NativeName, ArgLabel: string;
  out Value: Integer): Boolean;
begin
  if not isNumber(args[idx]) then
  begin
    RuntimeError(NativeName + '() argument ' + ArgLabel + ' must be a number.');
    Result := False;
  end
  else
  begin
    Value := Trunc(GetNumber(args[idx]));
    Result := True;
  end;
end;

// Widget handlers are always dispatched with either 0 args or 1 arg
// (the widget id — the Delphi Sender analogue). FireEdge picks the
// arity at dispatch time. Anything with 2+ declared parameters would
// end up called with 0 args and blow up mid-frame; rejecting the
// closure at registration time surfaces the mistake next to the
// offending code instead.
function ValidateHandlerArity(const NativeName, ArgLabel: string;
  const cb: TValue): Boolean;
var
  arity: Integer;
begin
  arity := pObjClosure(GetObject(cb))^.func^.arity;
  if (arity < 0) or (arity > 1) then
  begin
    RuntimeError(NativeName + '() ' + ArgLabel +
      ' must take 0 or 1 arguments.');
    Result := False;
  end
  else
    Result := True;
end;

function createButtonNative(argCount: integer; args: pValue): TValue;
var
  x, y, w, h, id: Integer;
  caption: string;
begin
  if (argCount < 5) or (argCount > 6) then
  begin
    RuntimeError('createButton() takes 5 or 6 arguments (x, y, w, h, caption [, onClick]).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'createButton', 'x', x) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 1, 'createButton', 'y', y) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 2, 'createButton', 'w', w) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 3, 'createButton', 'h', h) then Exit(CreateNilValue);
  if not isString(args[4]) then
  begin
    RuntimeError('createButton() caption must be a string.');
    Exit(CreateNilValue);
  end;
  if (argCount = 6) and not isClosure(args[5]) then
  begin
    RuntimeError('createButton() onClick must be a function.');
    Exit(CreateNilValue);
  end;
  if (argCount = 6) and
     not ValidateHandlerArity('createButton', 'onClick', args[5]) then
    Exit(CreateNilValue);
  caption := string(AsAnsiString(args[4]));
  id := WidgetsCreateButton(x, y, w, h, caption);
  if argCount = 6 then
    SetWidgetCallback(id, wcbClick, args[5]);
  Result := CreateNumber(id);
end;

// -- Kind guards ---------------------------------------------------------

// Guard for kind-specific accessors: unknown/stale ids stay silent
// no-ops (matching the generic setters — could be a race against
// freeWidget), but an EXISTING widget of the wrong kind is a script
// bug and errors loudly.
function RequireKind(id: Integer; Kind: TWidgetKind;
  const NativeName, KindDesc: string): Boolean;
begin
  Result := False;
  if not WidgetsExists(id) then Exit;
  if not WidgetsIsKind(id, Kind) then
  begin
    RuntimeError(NativeName + '(): widget ' + IntToStr(id) +
      ' is not ' + KindDesc + '.');
    Exit;
  end;
  Result := True;
end;

function RequireEdit(id: Integer; const NativeName: string): Boolean;
begin
  Result := RequireKind(id, wkEdit, NativeName, 'an edit box');
end;

// Shared body for the handler-attachment natives: <name>(id, fn)
// attaches or replaces the handler for one edge kind; nil detaches it
// and returns the widget to polling-only behaviour for that edge.
// widgetKind + KindDesc are the widget kind the caller’s edge belongs
// to (a click edge on a button, change on an edit/combo/radio, ...);
// RequireKind's semantics carry through unchanged — stale ids stay
// silent no-ops (matching the setters), wrong-kind ids error loudly.
function AttachCallbackNative(argCount: integer; args: pValue;
  const NativeName: string; kind: TWidgetCallbackKind;
  widgetKind: TWidgetKind; const KindDesc: string): TValue;
var
  id: Integer;
begin
  Result := CreateNilValue;
  if argCount <> 2 then
  begin
    RuntimeError(NativeName + '() takes 2 arguments (id, fn).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then Exit;
  // Validate the kind BEFORE looking at args[1] so wrong-kind ids
  // error even when detaching with nil — a slot mis-attached in the
  // wrong widget shouldn't be quietly “cleared” either.
  if WidgetsExists(id) and
     not RequireKind(id, widgetKind, NativeName, KindDesc) then Exit;
  if isNill(args[1]) then
    DetachWidgetCallback(id, kind)
  else if isClosure(args[1]) then
  begin
    if not ValidateHandlerArity(NativeName, 'fn', args[1]) then Exit;
    // Stale/unknown ids are silently ignored, matching the setters.
    if WidgetsExists(id) then
      SetWidgetCallback(id, kind, args[1]);
  end
  else
    RuntimeError(NativeName + '() fn must be a function or nil.');
end;

function setButtonOnClickNative(argCount: integer; args: pValue): TValue;
begin
  Result := AttachCallbackNative(argCount, args, 'setButtonOnClick',
    wcbClick, wkButton, 'a button');
end;

function setEditOnChangeNative(argCount: integer; args: pValue): TValue;
begin
  Result := AttachCallbackNative(argCount, args, 'setEditOnChange',
    wcbChange, wkEdit, 'an edit box');
end;

function setEditOnSubmitNative(argCount: integer; args: pValue): TValue;
begin
  Result := AttachCallbackNative(argCount, args, 'setEditOnSubmit',
    wcbSubmit, wkEdit, 'an edit box');
end;

function setWidgetCaptionNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
  caption: string;
begin
  if argCount <> 2 then
  begin
    RuntimeError('setWidgetCaption() takes 2 arguments (id, caption).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'setWidgetCaption', 'id', id) then Exit(CreateNilValue);
  if not isString(args[1]) then
  begin
    RuntimeError('setWidgetCaption() caption must be a string.');
    Exit(CreateNilValue);
  end;
  caption := string(AsAnsiString(args[1]));
  WidgetsSetCaption(id, caption);
  Result := CreateNilValue;
end;

function setWidgetBoundsNative(argCount: integer; args: pValue): TValue;
var
  id, x, y, w, h: Integer;
begin
  if argCount <> 5 then
  begin
    RuntimeError('setWidgetBounds() takes 5 arguments (id, x, y, w, h).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'setWidgetBounds', 'id', id) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 1, 'setWidgetBounds', 'x',  x)  then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 2, 'setWidgetBounds', 'y',  y)  then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 3, 'setWidgetBounds', 'w',  w)  then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 4, 'setWidgetBounds', 'h',  h)  then Exit(CreateNilValue);
  WidgetsSetBounds(id, x, y, w, h);
  Result := CreateNilValue;
end;

function setWidgetEnabledNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 2 then
  begin
    RuntimeError('setWidgetEnabled() takes 2 arguments (id, enabled).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'setWidgetEnabled', 'id', id) then Exit(CreateNilValue);
  // Any Lox value is coerceable to boolean via IsFalsey, matching how
  // the rest of the codebase treats "truthy" arguments (e.g. assert).
  WidgetsSetEnabled(id, not IsFalsey(args[1]));
  Result := CreateNilValue;
end;

function setWidgetVisibleNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 2 then
  begin
    RuntimeError('setWidgetVisible() takes 2 arguments (id, visible).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'setWidgetVisible', 'id', id) then Exit(CreateNilValue);
  WidgetsSetVisible(id, not IsFalsey(args[1]));
  Result := CreateNilValue;
end;

function freeWidgetNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('freeWidget() takes 1 argument (id).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'freeWidget', 'id', id) then Exit(CreateNilValue);
  WidgetsFree(id);
  RemoveCallbackSlot(id);
  Result := CreateNilValue;
end;

function clearWidgetsNative(argCount: integer; args: pValue): TValue;
begin
  WidgetsClear;
  ClearCallbackSlots;
  Result := CreateNilValue;
end;

function widgetCountNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(WidgetsCount);
end;

function widgetExistsNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('widgetExists() takes 1 argument (id).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'widgetExists', 'id', id) then Exit(CreateNilValue);
  Result := CreateBoolean(WidgetsExists(id));
end;

// Fire one widget's handler for one edge kind, if a handler is
// attached AND that edge completed this frame. Consuming the edge here
// is what routes it to the handler INSTEAD of the polling natives.
// Returns False when the handler raised a runtime error.
function FireEdge(id: Integer; kind: TWidgetCallbackKind): Boolean;
var
  slot: TCallbackSlot;
  cb, ret, idArg: TValue;
  res: TInterpretResult;
  fired: Boolean;
begin
  Result := True;
  // Look the slot up fresh every call — an earlier handler may have
  // freed the widget (destroying the slot object) or swapped handlers.
  if (FCallbackSlots = nil) or
     not FCallbackSlots.TryGetValue(id, slot) then Exit;
  cb := slot.Callbacks[kind];
  if not isClosure(cb) then Exit;
  case kind of
    wcbClick:  fired := WidgetsConsumeClick(id);
    wcbChange: fired := WidgetsConsumeChanged(id);
  else         fired := WidgetsConsumeSubmitted(id);
  end;
  if not fired then Exit;
  // fun() or fun(id): pass the id when the handler declares a
  // parameter (the Delphi Sender analogue — lets one handler serve
  // several widgets).
  if pObjClosure(GetObject(cb))^.func^.arity = 1 then
  begin
    idArg := CreateNumber(id);
    res := InvokeCallback(cb, [idArg], ret);
  end
  else
    res := InvokeCallback(cb, [], ret);
  Result := res.code = INTERPRET_OK;
end;

function updateWidgetsNative(argCount: integer; args: pValue): TValue;
var
  ids: TArray<Integer>;
  i: Integer;
begin
  Result := CreateNilValue;
  if FDispatchingCallbacks then
  begin
    RuntimeError('updateWidgets() cannot be called from inside a widget callback.');
    Exit;
  end;

  WidgetsUpdate;

  // Dispatch handlers for edges that completed this frame. Snapshot
  // the ids first: a handler may create or free widgets (mutating the
  // slot table) while we iterate. Per widget the order is click,
  // change, submit — typing then Enter in one frame fires onChange
  // before onSubmit.
  if (FCallbackSlots = nil) or (FCallbackSlots.Count = 0) then Exit;
  ids := FCallbackSlots.Keys.ToArray;
  FDispatchingCallbacks := True;
  try
    for i := 0 to High(ids) do
    begin
      if not FireEdge(ids[i], wcbClick) then Exit;   // error already set
      if not FireEdge(ids[i], wcbChange) then Exit;
      if not FireEdge(ids[i], wcbSubmit) then Exit;
    end;
  finally
    FDispatchingCallbacks := False;
  end;
end;

function drawWidgetsNative(argCount: integer; args: pValue): TValue;
begin
  WidgetsDrawAll;
  Result := CreateNilValue;
end;

// State accessor shared by buttonClicked / buttonHover / buttonPressed /
// etc. Extracted because they all share the same shape: one-arg id,
// dispatch to a query function, return boolean. This keeps each native
// body a two-liner.
type
  TBoolWidgetQuery = function(Id: Integer): Boolean;

function BoolQueryNative(argCount: integer; args: pValue;
  const NativeName: string; Query: TBoolWidgetQuery): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError(NativeName + '() takes 1 argument (id).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then Exit(CreateNilValue);
  Result := CreateBoolean(Query(id));
end;

// Same shape as BoolQueryNative but kind-guarded: used by the edge-
// CONSUMING natives (editChanged / editSubmitted / comboChanged /
// radioChanged) so a wrong-kind id doesn't silently read-and-clear
// the base ChangedEdge belonging to a legitimate widget of another
// kind. Stale/unknown ids stay silent (RequireKind's usual contract),
// matching every other setter.
function KindConsumeEdgeNative(argCount: integer; args: pValue;
  const NativeName: string; Query: TBoolWidgetQuery;
  Kind: TWidgetKind; const KindDesc: string): TValue;
var
  id: Integer;
begin
  Result := CreateBoolean(False);
  if argCount <> 1 then
  begin
    RuntimeError(NativeName + '() takes 1 argument (id).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then Exit;
  if WidgetsExists(id) and
     not RequireKind(id, Kind, NativeName, KindDesc) then Exit;
  Result := CreateBoolean(Query(id));
end;

function buttonClickedNative(argCount: integer; args: pValue): TValue;
begin
  // Consume-once: the underlying call clears the edge after reading.
  Result := BoolQueryNative(argCount, args, 'buttonClicked', WidgetsConsumeClick);
end;

function buttonHoverNative(argCount: integer; args: pValue): TValue;
begin
  Result := BoolQueryNative(argCount, args, 'buttonHover', WidgetsIsHover);
end;

function buttonPressedNative(argCount: integer; args: pValue): TValue;
begin
  Result := BoolQueryNative(argCount, args, 'buttonPressed', WidgetsIsPressed);
end;

function widgetEnabledNative(argCount: integer; args: pValue): TValue;
begin
  Result := BoolQueryNative(argCount, args, 'widgetEnabled', WidgetsIsEnabled);
end;

function widgetVisibleNative(argCount: integer; args: pValue): TValue;
begin
  Result := BoolQueryNative(argCount, args, 'widgetVisible', WidgetsIsVisible);
end;

// -- Edit-box natives --------------------------------------------------

function createEditNative(argCount: integer; args: pValue): TValue;
var
  x, y, w, h, id: Integer;
  text: string;
begin
  if (argCount < 4) or (argCount > 5) then
  begin
    RuntimeError('createEdit() takes 4 or 5 arguments (x, y, w, h [, text]).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'createEdit', 'x', x) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 1, 'createEdit', 'y', y) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 2, 'createEdit', 'w', w) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 3, 'createEdit', 'h', h) then Exit(CreateNilValue);
  text := '';
  if argCount = 5 then
  begin
    if not isString(args[4]) then
    begin
      RuntimeError('createEdit() text must be a string.');
      Exit(CreateNilValue);
    end;
    text := string(AsAnsiString(args[4]));
  end;
  id := WidgetsCreateEdit(x, y, w, h, text);
  Result := CreateNumber(id);
end;

function getEditTextNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError('getEditText() takes 1 argument (id).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'getEditText', 'id', id) then Exit(CreateNilValue);
  if WidgetsExists(id) and not WidgetsIsKind(id, wkEdit) then
  begin
    RuntimeError('getEditText(): widget ' + IntToStr(id) +
      ' is not an edit box.');
    Exit(CreateNilValue);
  end;
  // Unknown/stale id falls through to '' (getter default).
  Result := StringToValue(CreateString(AnsiString(WidgetsGetText(id)),
    VM.MemTracker));
end;

function setEditTextNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  Result := CreateNilValue;
  if argCount <> 2 then
  begin
    RuntimeError('setEditText() takes 2 arguments (id, text).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, 'setEditText', 'id', id) then Exit;
  if not isString(args[1]) then
  begin
    RuntimeError('setEditText() text must be a string.');
    Exit;
  end;
  if RequireEdit(id, 'setEditText') then
    WidgetsSetText(id, string(AsAnsiString(args[1])));
end;

function setEditMaxLengthNative(argCount: integer; args: pValue): TValue;
var
  id, maxLen: Integer;
begin
  Result := CreateNilValue;
  if argCount <> 2 then
  begin
    RuntimeError('setEditMaxLength() takes 2 arguments (id, maxLength).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, 'setEditMaxLength', 'id', id) then Exit;
  if not ArgNumOrErr(args, 1, 'setEditMaxLength', 'maxLength', maxLen) then Exit;
  if RequireEdit(id, 'setEditMaxLength') then
    WidgetsSetMaxLength(id, maxLen);
end;

function editChangedNative(argCount: integer; args: pValue): TValue;
begin
  // Consume-once, like buttonClicked, but kind-guarded so it can't
  // clear a combo/radio's ChangedEdge from under them.
  Result := KindConsumeEdgeNative(argCount, args, 'editChanged',
    WidgetsConsumeChanged, wkEdit, 'an edit box');
end;

function editSubmittedNative(argCount: integer; args: pValue): TValue;
begin
  Result := KindConsumeEdgeNative(argCount, args, 'editSubmitted',
    WidgetsConsumeSubmitted, wkEdit, 'an edit box');
end;

// -- Item-list natives (shared bodies for combo box and radio group) ----
// Both kinds carry an items list + single selection (TLoxListWidget),
// so each operation has one parameterized body and a thin kind-guarded
// wrapper per widget kind.

function ListAddItemNative(argCount: integer; args: pValue;
  const NativeName: string; Kind: TWidgetKind; const KindDesc: string): TValue;
var
  id: Integer;
begin
  Result := CreateNilValue;
  if argCount <> 2 then
  begin
    RuntimeError(NativeName + '() takes 2 arguments (id, text).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then Exit;
  if not isString(args[1]) then
  begin
    RuntimeError(NativeName + '() text must be a string.');
    Exit;
  end;
  if RequireKind(id, Kind, NativeName, KindDesc) then
    Result := CreateNumber(
      WidgetsListAddItem(id, string(AsAnsiString(args[1]))));
end;

function ListClearItemsNative(argCount: integer; args: pValue;
  const NativeName: string; Kind: TWidgetKind; const KindDesc: string): TValue;
var
  id: Integer;
begin
  Result := CreateNilValue;
  if argCount <> 1 then
  begin
    RuntimeError(NativeName + '() takes 1 argument (id).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then Exit;
  if RequireKind(id, Kind, NativeName, KindDesc) then
    WidgetsListClearItems(id);
end;

function ListItemCountNative(argCount: integer; args: pValue;
  const NativeName: string; Kind: TWidgetKind;
  const KindDesc: string): TValue;
var
  id: Integer;
begin
  Result := CreateNumber(0);
  if argCount <> 1 then
  begin
    RuntimeError(NativeName + '() takes 1 argument (id).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then
    Exit(CreateNilValue);
  // Stale ids: RequireKind returns False silently, we fall through to
  // the 0 default. Wrong-kind ids error loudly.
  if WidgetsExists(id) and
     not RequireKind(id, Kind, NativeName, KindDesc) then Exit;
  Result := CreateNumber(WidgetsListItemCount(id));
end;

function GetListIndexNative(argCount: integer; args: pValue;
  const NativeName: string; Kind: TWidgetKind;
  const KindDesc: string): TValue;
var
  id: Integer;
begin
  Result := CreateNumber(-1);
  if argCount <> 1 then
  begin
    RuntimeError(NativeName + '() takes 1 argument (id).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then
    Exit(CreateNilValue);
  if WidgetsExists(id) and
     not RequireKind(id, Kind, NativeName, KindDesc) then Exit;
  Result := CreateNumber(WidgetsGetListIndex(id));
end;

function SetListIndexNative(argCount: integer; args: pValue;
  const NativeName: string; Kind: TWidgetKind; const KindDesc: string): TValue;
var
  id, index: Integer;
begin
  Result := CreateNilValue;
  if argCount <> 2 then
  begin
    RuntimeError(NativeName + '() takes 2 arguments (id, index).');
    Exit;
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then Exit;
  if not ArgNumOrErr(args, 1, NativeName, 'index', index) then Exit;
  if RequireKind(id, Kind, NativeName, KindDesc) then
    WidgetsSetListIndex(id, index);
end;

function GetListTextNative(argCount: integer; args: pValue;
  const NativeName: string; Kind: TWidgetKind; const KindDesc: string): TValue;
var
  id: Integer;
begin
  if argCount <> 1 then
  begin
    RuntimeError(NativeName + '() takes 1 argument (id).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, NativeName, 'id', id) then Exit(CreateNilValue);
  if WidgetsExists(id) and not WidgetsIsKind(id, Kind) then
  begin
    RuntimeError(NativeName + '(): widget ' + IntToStr(id) +
      ' is not ' + KindDesc + '.');
    Exit(CreateNilValue);
  end;
  Result := StringToValue(CreateString(AnsiString(WidgetsGetListText(id)),
    VM.MemTracker));
end;

// -- Combo-box natives ---------------------------------------------------

function createComboNative(argCount: integer; args: pValue): TValue;
var
  x, y, w, h, id: Integer;
begin
  if argCount <> 4 then
  begin
    RuntimeError('createCombo() takes 4 arguments (x, y, w, h).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'createCombo', 'x', x) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 1, 'createCombo', 'y', y) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 2, 'createCombo', 'w', w) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 3, 'createCombo', 'h', h) then Exit(CreateNilValue);
  id := WidgetsCreateCombo(x, y, w, h);
  Result := CreateNumber(id);
end;

function addComboItemNative(argCount: integer; args: pValue): TValue;
begin
  Result := ListAddItemNative(argCount, args, 'addComboItem', wkCombo, 'a combo box');
end;

function clearComboItemsNative(argCount: integer; args: pValue): TValue;
begin
  Result := ListClearItemsNative(argCount, args, 'clearComboItems', wkCombo, 'a combo box');
end;

function comboItemCountNative(argCount: integer; args: pValue): TValue;
begin
  Result := ListItemCountNative(argCount, args, 'comboItemCount',
    wkCombo, 'a combo box');
end;

function getComboIndexNative(argCount: integer; args: pValue): TValue;
begin
  Result := GetListIndexNative(argCount, args, 'getComboIndex',
    wkCombo, 'a combo box');
end;

function setComboIndexNative(argCount: integer; args: pValue): TValue;
begin
  Result := SetListIndexNative(argCount, args, 'setComboIndex', wkCombo, 'a combo box');
end;

function getComboTextNative(argCount: integer; args: pValue): TValue;
begin
  Result := GetListTextNative(argCount, args, 'getComboText', wkCombo, 'a combo box');
end;

function comboChangedNative(argCount: integer; args: pValue): TValue;
begin
  // Same consume-once edge as editChanged — ChangedEdge lives on the
  // widget base class, so kind-guarded consumption keeps siblings
  // from clearing each other's edges.
  Result := KindConsumeEdgeNative(argCount, args, 'comboChanged',
    WidgetsConsumeChanged, wkCombo, 'a combo box');
end;

function setComboOnChangeNative(argCount: integer; args: pValue): TValue;
begin
  Result := AttachCallbackNative(argCount, args, 'setComboOnChange',
    wcbChange, wkCombo, 'a combo box');
end;

// -- Radio-group natives ---------------------------------------------------

function createRadioGroupNative(argCount: integer; args: pValue): TValue;
var
  x, y, w, h, id: Integer;
  caption: string;
begin
  if argCount <> 5 then
  begin
    RuntimeError('createRadioGroup() takes 5 arguments (x, y, w, h, caption).');
    Exit(CreateNilValue);
  end;
  if not ArgNumOrErr(args, 0, 'createRadioGroup', 'x', x) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 1, 'createRadioGroup', 'y', y) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 2, 'createRadioGroup', 'w', w) then Exit(CreateNilValue);
  if not ArgNumOrErr(args, 3, 'createRadioGroup', 'h', h) then Exit(CreateNilValue);
  if not isString(args[4]) then
  begin
    RuntimeError('createRadioGroup() caption must be a string.');
    Exit(CreateNilValue);
  end;
  caption := string(AsAnsiString(args[4]));
  id := WidgetsCreateRadioGroup(x, y, w, h, caption);
  Result := CreateNumber(id);
end;

function addRadioItemNative(argCount: integer; args: pValue): TValue;
begin
  Result := ListAddItemNative(argCount, args, 'addRadioItem', wkRadioGroup, 'a radio group');
end;

function clearRadioItemsNative(argCount: integer; args: pValue): TValue;
begin
  Result := ListClearItemsNative(argCount, args, 'clearRadioItems', wkRadioGroup, 'a radio group');
end;

function radioItemCountNative(argCount: integer; args: pValue): TValue;
begin
  Result := ListItemCountNative(argCount, args, 'radioItemCount',
    wkRadioGroup, 'a radio group');
end;

function getRadioIndexNative(argCount: integer; args: pValue): TValue;
begin
  Result := GetListIndexNative(argCount, args, 'getRadioIndex',
    wkRadioGroup, 'a radio group');
end;

function setRadioIndexNative(argCount: integer; args: pValue): TValue;
begin
  Result := SetListIndexNative(argCount, args, 'setRadioIndex', wkRadioGroup, 'a radio group');
end;

function getRadioTextNative(argCount: integer; args: pValue): TValue;
begin
  Result := GetListTextNative(argCount, args, 'getRadioText', wkRadioGroup, 'a radio group');
end;

function radioChangedNative(argCount: integer; args: pValue): TValue;
begin
  Result := KindConsumeEdgeNative(argCount, args, 'radioChanged',
    WidgetsConsumeChanged, wkRadioGroup, 'a radio group');
end;

function setRadioOnChangeNative(argCount: integer; args: pValue): TValue;
begin
  Result := AttachCallbackNative(argCount, args, 'setRadioOnChange',
    wcbChange, wkRadioGroup, 'a radio group');
end;

// -- Focus natives -----------------------------------------------------

function focusWidgetNative(argCount: integer; args: pValue): TValue;
var
  id: Integer;
begin
  Result := CreateNilValue;
  if argCount <> 1 then
  begin
    RuntimeError('focusWidget() takes 1 argument (id, or nil/0 to clear focus).');
    Exit;
  end;
  if isNill(args[0]) then
  begin
    WidgetsSetFocus(0);
    Exit;
  end;
  if not ArgNumOrErr(args, 0, 'focusWidget', 'id', id) then Exit;
  WidgetsSetFocus(id);
end;

function focusedWidgetNative(argCount: integer; args: pValue): TValue;
begin
  Result := CreateNumber(WidgetsGetFocus);
end;

procedure RegisterWidgetNatives;
begin
  // Reset any widgets left over from a previous script run so IDs
  // don't leak between runs — same lifecycle rule as
  // RegisterCanvasNatives clearing sprites / tilemaps / surfaces.
  // ClearCallbackSlots also drops handler TValues that belonged to the
  // previous (freed) VM before the new VM can ever see them. This is
  // the only sanctioned place to reset the widget ID counter; the
  // mid-session clearWidgets() native uses WidgetsClear, which
  // preserves FNextId so stale ids stay dead across a clear.
  WidgetsResetSession;
  ClearCallbackSlots;

  // Constructors (one per widget kind).
  defineNative('createButton',     createButtonNative,     -1);  // 5-6 args
  defineNative('createEdit',       createEditNative,       -1);  // 4-5 args
  defineNative('createCombo',      createComboNative,      4);
  defineNative('createRadioGroup', createRadioGroupNative, 5);

  // Generic widget operations — canonical names, valid for any kind.
  defineNative('setWidgetCaption', setWidgetCaptionNative, 2);
  defineNative('setWidgetBounds',  setWidgetBoundsNative,  5);
  defineNative('setWidgetEnabled', setWidgetEnabledNative, 2);
  defineNative('setWidgetVisible', setWidgetVisibleNative, 2);
  defineNative('freeWidget',       freeWidgetNative,       1);
  defineNative('widgetEnabled',    widgetEnabledNative,    1);
  defineNative('widgetVisible',    widgetVisibleNative,    1);
  defineNative('clearWidgets',     clearWidgetsNative,     0);
  defineNative('widgetCount',      widgetCountNative,      0);
  defineNative('widgetExists',     widgetExistsNative,     1);
  defineNative('updateWidgets',    updateWidgetsNative,    0);
  defineNative('drawWidgets',      drawWidgetsNative,      0);

  // Focus.
  defineNative('focusWidget',      focusWidgetNative,      1);
  defineNative('focusedWidget',    focusedWidgetNative,    0);

  // Button-specific operations.
  defineNative('setButtonOnClick', setButtonOnClickNative, 2);
  defineNative('buttonClicked',    buttonClickedNative,    1);
  defineNative('buttonHover',      buttonHoverNative,      1);
  defineNative('buttonPressed',    buttonPressedNative,    1);

  // Edit-box operations.
  defineNative('getEditText',      getEditTextNative,      1);
  defineNative('setEditText',      setEditTextNative,      2);
  defineNative('setEditMaxLength', setEditMaxLengthNative, 2);
  defineNative('editChanged',      editChangedNative,      1);
  defineNative('editSubmitted',    editSubmittedNative,    1);
  defineNative('setEditOnChange',  setEditOnChangeNative,  2);
  defineNative('setEditOnSubmit',  setEditOnSubmitNative,  2);

  // Combo-box operations.
  defineNative('addComboItem',     addComboItemNative,     2);
  defineNative('clearComboItems',  clearComboItemsNative,  1);
  defineNative('comboItemCount',   comboItemCountNative,   1);
  defineNative('getComboIndex',    getComboIndexNative,    1);
  defineNative('setComboIndex',    setComboIndexNative,    2);
  defineNative('getComboText',     getComboTextNative,     1);
  defineNative('comboChanged',     comboChangedNative,     1);
  defineNative('setComboOnChange', setComboOnChangeNative, 2);

  // Radio-group operations.
  defineNative('addRadioItem',     addRadioItemNative,     2);
  defineNative('clearRadioItems',  clearRadioItemsNative,  1);
  defineNative('radioItemCount',   radioItemCountNative,   1);
  defineNative('getRadioIndex',    getRadioIndexNative,    1);
  defineNative('setRadioIndex',    setRadioIndexNative,    2);
  defineNative('getRadioText',     getRadioTextNative,     1);
  defineNative('radioChanged',     radioChangedNative,     1);
  defineNative('setRadioOnChange', setRadioOnChangeNative, 2);

  // Compatibility aliases: the generic ops originally shipped under
  // button-specific names. Same native functions, so behaviour is
  // identical; error messages report the canonical name.
  defineNative('setButtonCaption', setWidgetCaptionNative, 2);
  defineNative('setButtonBounds',  setWidgetBoundsNative,  5);
  defineNative('setButtonEnabled', setWidgetEnabledNative, 2);
  defineNative('setButtonVisible', setWidgetVisibleNative, 2);
  defineNative('freeButton',       freeWidgetNative,       1);
  defineNative('buttonEnabled',    widgetEnabledNative,    1);
  defineNative('buttonVisible',    widgetVisibleNative,    1);
end;

initialization

finalization
  FreeAndNil(FCallbackSlots);

end.
