unit LoxWidgets;

// ============================================================
// LoxWidgets — a retained-mode native widget graphics package.
//
// This is the Delphi-side widget engine. Widgets are drawn into the
// LoxCanvas back buffer using GDI (Windows.DrawFrameControl for the
// bevel + GDI text for the caption) so they look like real Delphi VCL
// controls on the classic Windows theme — TLoxButton is a faithful
// analogue of TButton in appearance and behaviour.
//
// This unit is UI-framework agnostic: it does not talk to Lox, the VM,
// or any script surface. It just exposes an ID-keyed API you can drive
// from anything — host code, natives, whatever. WidgetNatives.pas is
// the thin Lox wrapper on top.
//
// Model
// -----
// Widgets are retained (created once, alive until freed or WidgetsClear)
// and rendered every frame via WidgetsDrawAll. IDs are 1-based and
// never recycled within a session so scripts can safely hang on to
// stale IDs (WidgetsExists returns False for a freed one).
//
// Each widget kind is a TLoxWidget subclass that overrides Draw (its
// appearance) and, when it needs more than the standard pointer
// gesture, HandleInput (its behaviour). Shared fields and the standard
// hover/capture/click state machine live in the base class.
//
// Z-order and input routing
// -------------------------
// List order is creation order is z-order: WidgetsDrawAll paints
// first-created first, so later widgets draw on top. Input goes the
// other way — WidgetsUpdate offers the pointer topmost-first and the
// first widget hit CLAIMS it, so widgets underneath see the pointer as
// "elsewhere" and overlaps resolve to whatever is drawn on top. While
// a widget holds an active capture (mouse went down on it and is still
// down), it receives the pointer exclusively, matching Windows mouse
// capture semantics.
//
// Popup overlays (an open combo dropdown) get the same exclusivity:
// while a widget reports PopupActive, it is routed the pointer first
// and everything else sees it as elsewhere — so a press outside the
// popup folds it closed WITHOUT also activating whatever was under
// the press, exactly like a real dropped combo holding mouse capture.
// WidgetsDrawAll paints the popup-active widget last so its overlay
// sits above every other widget regardless of creation order.
//
// Keyboard focus
// --------------
// At most one widget holds keyboard focus (FFocusedId); only it acts
// on the frame's typed characters and navigation keys. A press on a
// focus-accepting widget (WantsFocus, e.g. an edit box) focuses it; a
// press on any other widget or on empty canvas clears focus — the
// caret never keeps blinking in an edit you just clicked away from.
// Freeing, hiding or disabling the focused widget also clears focus.
// Note the engine does not steal keys from script polling: while an
// edit is focused, keyPressed()/keyHeld() still see everything, so
// scripts mixing gameplay keys with widgets should gate game input on
// focusedWidget() == 0.
//
// Per-frame lifecycle:
//   1. The host pumps messages (runGameLoop does this before every
//      onFrame call; a manual loop calls processMessages()). The pump
//      resets per-frame edge-trigger state via ClearFrameEdges.
//   2. Host calls WidgetsUpdate — samples the logical mouse into a
//      TFrameInput snapshot and routes it to every widget.
//   3. Host does its own drawing.
//   4. Host calls WidgetsDrawAll — blits each visible widget over the
//      current canvas back buffer.
//   5. Host calls present().
//
// Click detection follows the standard button-release-inside contract:
// the click event fires on mouse-up while the pointer is inside a
// widget AND that widget received the mouse-down that started the
// gesture (i.e. dragging off cancels the click). Consumers see this
// via WidgetsConsumeClick, which reads the edge and clears it.
//
// Adding new widget kinds
// -----------------------
// 1. Extend TWidgetKind with a new value (wkEdit, wkCombo, ...).
// 2. Subclass TLoxWidget; add the kind's state as public fields and
//    set Kind in the constructor.
// 3. Override Draw. Override HandleInput only when the widget needs
//    more than the standard pointer gesture — call inherited first so
//    the hover/capture/click state machine still runs. Override
//    HitTest if the widget's interactive area can exceed its bounds
//    (an open dropdown).
// 4. Add ID-keyed setters/getters in the API section below.
// 5. Expose from WidgetNatives.pas as needed.
// ============================================================

interface

uses
  Winapi.Windows, System.Classes, Vcl.Graphics;

type
  // Widget shape tag, kept alongside the class type for cheap
  // introspection and kind-guards in the natives layer.
  TWidgetKind = (wkButton, wkEdit, wkCombo, wkRadioGroup);

  // Per-frame input snapshot. WidgetsUpdate builds one and hands it to
  // every widget's HandleInput. New input sources (mouse wheel, ...)
  // become new fields here rather than new parameters on every
  // widget's input method.
  TFrameInput = record
    MouseX, MouseY: Integer;   // logical canvas coordinates
    LeftDown: Boolean;         // left button held this frame
    LeftPressedEdge: Boolean;  // left went down since last frame
    LeftReleasedEdge: Boolean; // left went up since last frame
    // WM_CHAR stream this frame (shift/layout applied, autorepeat
    // included; control chars #8/#13 arrive here too). Only the
    // focused widget should act on these.
    Chars: string;
    // Edge-triggered navigation keys (no autorepeat — the canvas edge
    // set only fires on genuine first-down).
    NavLeft, NavRight, NavHome, NavEnd, NavDelete: Boolean;
    NavUp, NavDown: Boolean;
  end;

  // Widget base class. Public fields are intentional — this is an
  // internal container, not part of any public API. Callers touch
  // widgets through the ID-keyed procedures below.
  TLoxWidget = class
  public
    Kind: TWidgetKind;
    Id: Integer;
    X, Y, W, H: Integer;
    Caption: string;
    Enabled: Boolean;
    Visible: Boolean;
    // Live input state, refreshed every WidgetsUpdate call.
    Hover: Boolean;       // pointer currently over the widget
    Pressed: Boolean;     // mouse-down originated on this widget AND
                          // pointer is still inside (visual "pushed")
    // Tracks whether this widget owns the current mouse-down gesture,
    // even after the pointer drags off. Used to fire the click on
    // release-inside only if the gesture started here.
    Capturing: Boolean;
    // Edge flag: set once when a full press+release-inside completes,
    // cleared by WidgetsConsumeClick. Non-consuming peek is WidgetsIsClicked.
    ClickedEdge: Boolean;
    // Edge flag for value-carrying widgets (edit text mutated, combo
    // selection changed): cleared by WidgetsConsumeChanged. Never set
    // by programmatic setters, only by user interaction.
    ChangedEdge: Boolean;
    constructor Create(AId: Integer);
    // True when the LOGICAL point lands on the widget. Uses the same
    // [x..x+w) [y..y+h) half-open convention as fillRect / drawRect,
    // so the edge you see drawn is the edge you can click on. Virtual
    // so a widget whose interactive area can exceed its bounds (an
    // open dropdown) can extend it.
    function HitTest(PX, PY: Integer): Boolean; virtual;
    // Whether a press on this widget moves keyboard focus to it (see
    // the focus model in the unit header). False for buttons, True
    // for text-editing widgets.
    function WantsFocus: Boolean; virtual;
    // True while this widget holds the keyboard focus.
    function IsFocused: Boolean;
    // Standard pointer gesture: hover, capture-on-press, click on
    // release-inside. PointerClaimed implements topmost-first hit
    // priority (see unit header). Subclasses that override this should
    // call inherited first.
    procedure HandleInput(const Frame: TFrameInput;
      var PointerClaimed: Boolean); virtual;
    // Paint onto the canvas back buffer. Only called when Visible.
    procedure Draw(Bm: TBitmap); virtual; abstract;
    // Drop any in-flight gesture (widget hidden or disabled mid-drag).
    // Virtual so popup widgets can also fold their overlay away.
    procedure CancelGesture; virtual;
    // True while this widget shows a popup overlay (an open dropdown).
    // A popup-active widget receives the pointer exclusively and draws
    // above everything — see "Z-order and input routing" above.
    function PopupActive: Boolean; virtual;
  end;

  // Classic push button: Windows.DrawFrameControl bevel + centred
  // caption in the default GUI font. Behaviour is entirely the base
  // class's standard gesture.
  TLoxButton = class(TLoxWidget)
  public
    constructor Create(AId: Integer);
    procedure Draw(Bm: TBitmap); override;
  end;

  // Single-line edit box: classic sunken frame (DrawEdge EDGE_SUNKEN),
  // white fill, left-aligned text in the default GUI font, blinking
  // caret while focused. Click focuses and places the caret; typing
  // comes from the frame's WM_CHAR stream (so shift, layout and
  // autorepeat behave like a real TEdit); left/right/home/end/delete
  // navigate and edit (edge-triggered — no autorepeat on those yet).
  // Enter sets the submitted edge; text mutation sets the changed edge.
  TLoxEdit = class(TLoxWidget)
  private
    // Keep the caret inside the visible span, adjusting the pixel
    // scroll offset after any caret/text change.
    procedure EnsureCaretVisible;
    // Caret index (0..Length(Text)) nearest to a logical click X,
    // using the midpoint-of-glyph rule every edit control uses.
    function CaretFromPoint(PX: Integer): Integer;
  public
    Text: string;
    CaretPos: Integer;    // insertion point, 0 = before first char
    ScrollX: Integer;     // horizontal pixel scroll for long text
    MaxLength: Integer;   // 0 = unlimited
    // Consume-once edge, mirroring the base ChangedEdge (set on user
    // text mutation): set on Enter, cleared by
    // WidgetsConsumeSubmitted.
    SubmittedEdge: Boolean;
    constructor Create(AId: Integer);
    function WantsFocus: Boolean; override;
    procedure HandleInput(const Frame: TFrameInput;
      var PointerClaimed: Boolean); override;
    procedure Draw(Bm: TBitmap); override;
  end;

  // Shared base for widgets carrying an item list + single selection
  // (combo box, radio group). Owns the list; SelectIndex is the
  // user-interaction path and fires ChangedEdge, while the ID-keyed
  // programmatic setter assigns ItemIndex directly (no edge).
  TLoxListWidget = class(TLoxWidget)
  public
    Items: TStringList;
    ItemIndex: Integer;    // -1 = no selection
    constructor Create(AId: Integer);
    destructor Destroy; override;
    function WantsFocus: Boolean; override;
    // Change the selection as the USER: fires ChangedEdge when the
    // index actually changes. Out-of-range indices are ignored.
    procedure SelectIndex(NewIndex: Integer);
    // Hook invoked by WidgetsListClearItems after the list empties and
    // ItemIndex resets, for kind-specific state (combo folds).
    procedure ItemsCleared; virtual;
  end;

  // Drop-down combo box (csDropDownList style: pick from items, no
  // free text). Closed it renders as a sunken field + classic combo
  // arrow; open it overlays a bordered list below (flipping above when
  // the canvas bottom is too close). While open the engine routes the
  // pointer to it exclusively; a press outside folds it closed and is
  // swallowed, matching a real dropped combo's mouse capture.
  // Keyboard while focused: up/down move the selection (closed) or the
  // highlight (open); Enter commits the highlight; Escape folds.
  // Scrolling isn't modelled yet (no mouse wheel in the canvas), so
  // only the first MaxDropItems items are reachable while open.
  TLoxCombo = class(TLoxListWidget)
  private
    function ItemHeight: Integer;
    function VisibleItems: Integer;  // rows the dropdown shows (>= 1)
    function DropdownRect: TRect;
    // Item row index under a logical point, -1 when outside the rows.
    function ItemAtPoint(PX, PY: Integer): Integer;
  public
    DroppedDown: Boolean;
    HoverIndex: Integer;   // highlighted row while open
    MaxDropItems: Integer; // visible dropdown rows cap
    constructor Create(AId: Integer);
    function HitTest(PX, PY: Integer): Boolean; override;
    function PopupActive: Boolean; override;
    procedure CancelGesture; override;
    procedure ItemsCleared; override;
    procedure HandleInput(const Frame: TFrameInput;
      var PointerClaimed: Boolean); override;
    procedure Draw(Bm: TBitmap); override;
  end;

  // Radio-button group, one widget for the whole group (the Delphi
  // TRadioGroup analogue) — mutual exclusivity is just ItemIndex, no
  // inter-widget coordination. Renders a BTNFACE panel with an etched
  // group frame, the caption on the frame's top edge, and one classic
  // radio glyph + label per item in a single column. Click an item to
  // select it (fires the changed edge); up/down move the selection
  // while focused. Items that don't fit the height are neither drawn
  // nor clickable — size the group to its items (no scrolling).
  TLoxRadioGroup = class(TLoxListWidget)
  private
    function RowHeight: Integer;
    function ItemsTop: Integer;
    // Item row index under a logical point, -1 when outside.
    function ItemAtPoint(PX, PY: Integer): Integer;
  public
    constructor Create(AId: Integer);
    procedure HandleInput(const Frame: TFrameInput;
      var PointerClaimed: Boolean); override;
    procedure Draw(Bm: TBitmap); override;
  end;

// -- ID-keyed API. Invalid IDs are ignored on setters and return
//    sensible defaults on getters, mirroring how canvas natives treat
//    stale sprite/tilemap IDs.

// Create a new button. Returns the new widget's ID (>= 1). The button
// is created in the normal state (enabled, visible, not pressed).
function WidgetsCreateButton(X, Y, W, H: Integer;
  const Caption: string): Integer;

// Create a new single-line edit box with the given initial text.
// Returns the new widget's ID (>= 1). Caret starts after the text.
function WidgetsCreateEdit(X, Y, W, H: Integer;
  const Text: string): Integer;

// Edit-box accessors. Setters silently ignore unknown or non-edit ids;
// getters return '' / False for them (the natives layer adds louder
// kind-guard errors where silence would mislead). WidgetsSetText moves
// the caret to the end and does NOT set the changed edge.
procedure WidgetsSetText(Id: Integer; const Text: string);
function WidgetsGetText(Id: Integer): string;
procedure WidgetsSetMaxLength(Id, MaxLen: Integer);
function WidgetsConsumeChanged(Id: Integer): Boolean;
function WidgetsConsumeSubmitted(Id: Integer): Boolean;

// Item-list widget constructors.
function WidgetsCreateCombo(X, Y, W, H: Integer): Integer;
function WidgetsCreateRadioGroup(X, Y, W, H: Integer;
  const Caption: string): Integer;

// Item-list accessors, shared by every TLoxListWidget kind (combo,
// radio group). Same guard convention as the edit accessors.
// WidgetsSetListIndex clamps to [-1 .. Count-1] and does NOT set the
// changed edge; user selection does. Item indices are 0-based.
function WidgetsListAddItem(Id: Integer; const Text: string): Integer; // new index, -1 if not a list widget
procedure WidgetsListClearItems(Id: Integer);
function WidgetsListItemCount(Id: Integer): Integer;
function WidgetsGetListIndex(Id: Integer): Integer;    // -1 = none
procedure WidgetsSetListIndex(Id, Index: Integer);
function WidgetsGetListText(Id: Integer): string;      // selected item, '' if none

// Keyboard focus. At most one widget is focused; only it receives the
// frame's typed characters and nav keys. Focus moves on click (see
// TLoxWidget.WantsFocus) or explicitly here. 0 = no focus. SetFocus
// ignores ids that don't exist, don't accept focus, or aren't
// enabled+visible.
function WidgetsGetFocus: Integer;
procedure WidgetsSetFocus(Id: Integer);

// Kind introspection for the natives layer's guards. False for
// unknown ids.
function WidgetsIsKind(Id: Integer; Kind: TWidgetKind): Boolean;

// Mutators. All silently ignore an unknown Id.
procedure WidgetsSetBounds(Id, X, Y, W, H: Integer);
procedure WidgetsSetCaption(Id: Integer; const Caption: string);
procedure WidgetsSetEnabled(Id: Integer; Enabled: Boolean);
procedure WidgetsSetVisible(Id: Integer; Visible: Boolean);

// Introspection.
function WidgetsExists(Id: Integer): Boolean;
function WidgetsCount: Integer;

// Free a specific widget or every widget. Freeing a currently-hovered
// widget mid-frame is safe — its slot is nilled out and Update/Draw
// skip nil entries.
procedure WidgetsFree(Id: Integer);
procedure WidgetsClear;

// Per-frame poll: builds a TFrameInput from LoxCanvas mouse state and
// routes it to every widget (capture-exclusive, then topmost-first).
procedure WidgetsUpdate;

// Render all visible widgets over the current canvas back buffer, in
// creation order (first created at the bottom).
procedure WidgetsDrawAll;

// True (once) if this widget saw a full click since the last call. The
// call clears the edge. Suitable for polling from a game loop.
function WidgetsConsumeClick(Id: Integer): Boolean;

// Non-consuming state peeks.
function WidgetsIsClicked(Id: Integer): Boolean;
function WidgetsIsHover(Id: Integer): Boolean;
function WidgetsIsPressed(Id: Integer): Boolean;
function WidgetsIsEnabled(Id: Integer): Boolean;
function WidgetsIsVisible(Id: Integer): Boolean;

implementation

uses
  System.SysUtils, System.Generics.Collections, Vcl.Themes,
  LoxCanvas;

var
  // Flat list of live widgets. Freed slots are nilled (not compacted)
  // so external IDs stay stable across frees. WidgetsClear resets
  // everything including the ID counter.
  FWidgets: TObjectList<TLoxWidget> = nil;
  // Monotonic ID allocator. 1-based so that scripts can use 0 as "no
  // widget" sentinel if they want. Reset by WidgetsClear.
  FNextId: Integer = 1;
  // Previous frame's held state for the primary (left) mouse button.
  // Used to detect the down->up edge for click completion without
  // being coupled to CanvasMouseButtonClickedEdge (which is a
  // down-edge trigger, not what we need for release-inside logic).
  FPrevLeftDown: Boolean = False;
  // Id of the widget holding keyboard focus; 0 = none. See the focus
  // model in the unit header.
  FFocusedId: Integer = 0;

// -- GUI-font text measurement ----------------------------------------
// Edit boxes need pixel widths of text prefixes (caret placement,
// scrolling) outside of any Draw call, so we keep one memory DC with
// DEFAULT_GUI_FONT selected for the unit's lifetime.

var
  FMeasureDC: HDC = 0;
  FGuiTextHeight: Integer = 0;

procedure EnsureMeasureDC;
var
  tm: TEXTMETRIC;
begin
  if FMeasureDC <> 0 then Exit;
  FMeasureDC := CreateCompatibleDC(0);
  SelectObject(FMeasureDC, GetStockObject(DEFAULT_GUI_FONT));
  GetTextMetrics(FMeasureDC, tm);
  FGuiTextHeight := tm.tmHeight;
end;

function MeasureGuiText(const S: string): Integer;
var
  sz: TSize;
begin
  if S = '' then Exit(0);
  EnsureMeasureDC;
  GetTextExtentPoint32W(FMeasureDC, PWideChar(S), Length(S), sz);
  Result := sz.cx;
end;

function GuiTextHeight: Integer;
begin
  EnsureMeasureDC;
  Result := FGuiTextHeight;
end;

{ TLoxWidget }

constructor TLoxWidget.Create(AId: Integer);
begin
  inherited Create;
  Id := AId;
  Enabled := True;
  Visible := True;
end;

function TLoxWidget.HitTest(PX, PY: Integer): Boolean;
begin
  Result := (PX >= X) and (PX < X + W) and
            (PY >= Y) and (PY < Y + H);
end;

function TLoxWidget.WantsFocus: Boolean;
begin
  Result := False;
end;

function TLoxWidget.IsFocused: Boolean;
begin
  Result := (Id <> 0) and (Id = FFocusedId);
end;

procedure TLoxWidget.CancelGesture;
begin
  Capturing := False;
  Pressed := False;
  Hover := False;
end;

function TLoxWidget.PopupActive: Boolean;
begin
  Result := False;
end;

procedure TLoxWidget.HandleInput(const Frame: TFrameInput;
  var PointerClaimed: Boolean);
var
  Inside: Boolean;
begin
  if not Visible then
  begin
    CancelGesture;
    Exit;
  end;

  // Topmost-first hit priority: the first widget the pointer lands on
  // claims it; anything underneath sees the pointer as elsewhere. A
  // disabled widget still claims — it covers what's under it, exactly
  // like a disabled control in VCL.
  Inside := (not PointerClaimed) and HitTest(Frame.MouseX, Frame.MouseY);
  if Inside then
    PointerClaimed := True;

  // Hover only reads as True on an enabled widget — disabled widgets
  // don't need the hover ring, matching how VCL suppresses hover
  // feedback on disabled controls.
  Hover := Inside and Enabled;

  if not Enabled then
  begin
    Pressed := False;
    Capturing := False;
    Exit;
  end;

  // Down edge: capture follows wherever the press landed. Assigning
  // (not or-ing) means a press elsewhere also cancels any stale capture
  // left by a press+release that both fell inside one frame — the
  // release edge is derived from held-state sampling in WidgetsUpdate,
  // so a sub-frame click is invisible and would otherwise leave
  // Capturing set, firing a phantom click on the next release-inside.
  if Frame.LeftPressedEdge then
  begin
    Capturing := Inside;
    // Click-to-focus: a press on a focus-accepting widget moves focus
    // to it; a press on any other widget clears focus (Windows-like —
    // clicking a button while typing in an edit dismisses the caret).
    // A press on empty canvas is handled in WidgetsUpdate.
    if Inside then
    begin
      if WantsFocus then
        FFocusedId := Id
      else
        FFocusedId := 0;
    end;
  end;

  // Up edge with active capture completes the gesture. Click fires
  // only if the pointer is still inside — matches release-inside
  // semantics used by every desktop toolkit.
  if Frame.LeftReleasedEdge and Capturing then
  begin
    if Inside then
      ClickedEdge := True;
    Capturing := False;
  end;

  // "Pressed" (visually pushed) is capture AND currently inside. This
  // is how VCL animates a TButton: dragging off releases the pushed
  // look, dragging back on re-applies it, and the button only fires
  // OnClick if you release while pushed.
  Pressed := Capturing and Inside;
end;

{ TLoxButton }

constructor TLoxButton.Create(AId: Integer);
begin
  inherited Create(AId);
  Kind := wkButton;
end;

// -- Rendering --------------------------------------------------------
// Widgets draw straight to the LoxCanvas back buffer's TCanvas using
// GDI. Choosing Windows.DrawFrameControl over hand-rolling the bevel
// colour tables gives us pixel-perfect classic Delphi output on
// Windows with virtually no code — and it also honours user colour
// scheme changes if the OS still has one live.
//
// Note: we intentionally do NOT go through the canvas pixel rasterizer
// (FCurrentPixel / ScanLine paths). That path is optimised for pixel
// art and doesn't know about GDI fonts or edge-drawing. GDI can share
// the 32-bit DIB with the rasterizer's direct bit writes, but ONLY
// with explicit synchronisation: GDI calls are batched per thread, so
// WidgetsDrawAll ends with GdiFlush before anything touches the bits
// directly again (see MSDN's DIB-section rules). Skipping that flush
// makes widget output apply out of order — flicker.

procedure TLoxButton.Draw(Bm: TBitmap);
var
  R, TextR: TRect;
  DC: HDC;
  State: UINT;
  SaveMode: Integer;
  SaveColor: TColorRef;
  OldFont: HGDIOBJ;
  Flags: UINT;
begin
  R.Left := X;
  R.Top := Y;
  R.Right := X + W;
  R.Bottom := Y + H;

  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;

  DC := Bm.Canvas.Handle;

  // DFCS_BUTTONPUSH is Windows' classic TButton style — the same one
  // DrawFrameControl uses for BS_PUSHBUTTON. DFCS_PUSHED sinks the
  // bevel and shifts content; DFCS_INACTIVE greys the caption edging.
  State := DFCS_BUTTONPUSH;
  if Pressed then
    State := State or DFCS_PUSHED;
  if not Enabled then
    State := State or DFCS_INACTIVE;

  DrawFrameControl(DC, R, DFC_BUTTON, State);

  // Caption rendering. Use the system default GUI font (Tahoma / Segoe
  // UI depending on Windows version) so we look exactly like a
  // Delphi form at design time. GDI font selection is scoped to this
  // call: we save and restore the previous handle so subsequent
  // canvas natives (drawText etc.) that rely on TCanvas.Font aren't
  // affected. We touch the raw DC (not Bm.Canvas.Font) because
  // TCanvas caches its font handle and won't notice our SelectObject.

  if Caption <> '' then
  begin
    OldFont := SelectObject(DC, GetStockObject(DEFAULT_GUI_FONT));
    SaveMode := SetBkMode(DC, TRANSPARENT);
    if Enabled then
      SaveColor := SetTextColor(DC, ColorToRGB(clBtnText))
    else
      SaveColor := SetTextColor(DC, ColorToRGB(clGrayText));

    TextR := R;
    if Pressed then
    begin
      // Classic Windows shifts pushed-button content 1px down+right.
      OffsetRect(TextR, 1, 1);
    end;

    Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS or
             DT_NOPREFIX;
    DrawTextW(DC, PWideChar(Caption), Length(Caption), TextR, Flags);

    SetTextColor(DC, SaveColor);
    SetBkMode(DC, SaveMode);
    SelectObject(DC, OldFont);
  end;
end;

{ TLoxEdit }

const
  // Classic sunken frame is 2px (EDGE_SUNKEN), plus a small text inset
  // matching TEdit's client padding.
  EditBorder = 2;
  EditPad = 3;

constructor TLoxEdit.Create(AId: Integer);
begin
  inherited Create(AId);
  Kind := wkEdit;
end;

function TLoxEdit.WantsFocus: Boolean;
begin
  Result := True;
end;

function TLoxEdit.CaretFromPoint(PX: Integer): Integer;
var
  relX, i, prev, cur: Integer;
begin
  relX := PX - (X + EditBorder + EditPad) + ScrollX;
  if relX <= 0 then Exit(0);
  prev := 0;
  for i := 1 to Length(Text) do
  begin
    cur := MeasureGuiText(Copy(Text, 1, i));
    // Left half of a glyph puts the caret before it, right half after.
    if relX < (prev + cur) div 2 then
      Exit(i - 1);
    prev := cur;
  end;
  Result := Length(Text);
end;

procedure TLoxEdit.EnsureCaretVisible;
var
  innerW, caretX, maxScroll: Integer;
begin
  innerW := W - 2 * (EditBorder + EditPad);
  if innerW <= 1 then
  begin
    ScrollX := 0;
    Exit;
  end;
  // Never scroll past the point where the text's tail plus one caret
  // column is visible — shrinking text snaps the view back.
  maxScroll := MeasureGuiText(Text) - (innerW - 1);
  if maxScroll < 0 then maxScroll := 0;
  if ScrollX > maxScroll then ScrollX := maxScroll;

  caretX := MeasureGuiText(Copy(Text, 1, CaretPos));
  if caretX - ScrollX > innerW - 1 then
    ScrollX := caretX - (innerW - 1)
  else if caretX - ScrollX < 0 then
    ScrollX := caretX;
end;

procedure TLoxEdit.HandleInput(const Frame: TFrameInput;
  var PointerClaimed: Boolean);
var
  clickInside, changed: Boolean;
  i: Integer;
  ch: Char;
begin
  // Snapshot before inherited flips PointerClaimed: did this press
  // land on us? (Used for caret placement after the base class has
  // handled capture + focus.)
  clickInside := Frame.LeftPressedEdge and Visible and Enabled and
                 (not PointerClaimed) and HitTest(Frame.MouseX, Frame.MouseY);

  inherited HandleInput(Frame, PointerClaimed);

  if clickInside then
  begin
    CaretPos := CaretFromPoint(Frame.MouseX);
    EnsureCaretVisible;
  end;

  // Keyboard editing: focused widgets only.
  if not (IsFocused and Visible and Enabled) then Exit;

  changed := False;

  // Navigation / delete (edge-triggered).
  if Frame.NavLeft and (CaretPos > 0) then Dec(CaretPos);
  if Frame.NavRight and (CaretPos < Length(Text)) then Inc(CaretPos);
  if Frame.NavHome then CaretPos := 0;
  if Frame.NavEnd then CaretPos := Length(Text);
  if Frame.NavDelete and (CaretPos < Length(Text)) then
  begin
    Delete(Text, CaretPos + 1, 1);
    changed := True;
  end;

  // Typed characters, in arrival order. Backspace and Enter travel in
  // the WM_CHAR stream (so both autorepeat like a real edit control).
  for i := 1 to Length(Frame.Chars) do
  begin
    ch := Frame.Chars[i];
    case ch of
      #8:
        if CaretPos > 0 then
        begin
          Delete(Text, CaretPos, 1);
          Dec(CaretPos);
          changed := True;
        end;
      #13:
        SubmittedEdge := True;
    else
      if (ch >= ' ') and ((MaxLength = 0) or (Length(Text) < MaxLength)) then
      begin
        Insert(string(ch), Text, CaretPos + 1);
        Inc(CaretPos);
        changed := True;
      end;
    end;
  end;

  if changed then
    ChangedEdge := True;
  EnsureCaretVisible;
end;

// Caret blink phase from the user's system blink rate. A disabled
// blink (0 / INFINITE) renders the caret steady rather than absent.
function CaretBlinkOn: Boolean;
var
  blink: Cardinal;
begin
  blink := GetCaretBlinkTime;
  if (blink = 0) or (blink = INFINITE) then
    Result := True
  else
    Result := (GetTickCount div blink) and 1 = 0;
end;

procedure TLoxEdit.Draw(Bm: TBitmap);
var
  R, Inner, CaretR: TRect;
  DC: HDC;
  OldFont: HGDIOBJ;
  SaveMode: Integer;
  SaveColor: TColorRef;
  textY, caretX: Integer;
begin
  R := Rect(X, Y, X + W, Y + H);
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;
  DC := Bm.Canvas.Handle;

  // EDGE_SUNKEN + BF_ADJUST draws the classic 2px sunken frame and
  // shrinks Inner to the client area — the same look as a VCL TEdit
  // with Ctl3D on the classic theme.
  Inner := R;
  DrawEdge(DC, Inner, EDGE_SUNKEN, BF_RECT or BF_ADJUST);
  if Enabled then
    FillRect(DC, Inner, GetSysColorBrush(COLOR_WINDOW))
  else
    FillRect(DC, Inner, GetSysColorBrush(COLOR_BTNFACE));

  OldFont := SelectObject(DC, GetStockObject(DEFAULT_GUI_FONT));
  SaveMode := SetBkMode(DC, TRANSPARENT);
  if Enabled then
    SaveColor := SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT))
  else
    SaveColor := SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));

  textY := Inner.Top + (Inner.Bottom - Inner.Top - GuiTextHeight) div 2;
  if Text <> '' then
    // ETO_CLIPPED confines glyphs to the client area even when the
    // view is scrolled into the middle of the string.
    ExtTextOutW(DC, Inner.Left + EditPad - ScrollX, textY, ETO_CLIPPED,
      @Inner, PWideChar(Text), Length(Text), nil);

  if IsFocused and Enabled and CaretBlinkOn then
  begin
    caretX := Inner.Left + EditPad - ScrollX +
              MeasureGuiText(Copy(Text, 1, CaretPos));
    if (caretX >= Inner.Left) and (caretX < Inner.Right) then
    begin
      CaretR := Rect(caretX, Inner.Top + 1, caretX + 1, Inner.Bottom - 1);
      FillRect(DC, CaretR, GetSysColorBrush(COLOR_WINDOWTEXT));
    end;
  end;

  SetTextColor(DC, SaveColor);
  SetBkMode(DC, SaveMode);
  SelectObject(DC, OldFont);
end;

{ TLoxListWidget }

constructor TLoxListWidget.Create(AId: Integer);
begin
  inherited Create(AId);
  Items := TStringList.Create;
  ItemIndex := -1;
end;

destructor TLoxListWidget.Destroy;
begin
  Items.Free;
  inherited;
end;

function TLoxListWidget.WantsFocus: Boolean;
begin
  Result := True;
end;

procedure TLoxListWidget.SelectIndex(NewIndex: Integer);
begin
  if (NewIndex >= 0) and (NewIndex < Items.Count) and
     (NewIndex <> ItemIndex) then
  begin
    ItemIndex := NewIndex;
    ChangedEdge := True;
  end;
end;

procedure TLoxListWidget.ItemsCleared;
begin
  // Base has nothing beyond what WidgetsListClearItems already reset.
end;

{ TLoxCombo }

constructor TLoxCombo.Create(AId: Integer);
begin
  inherited Create(AId);
  Kind := wkCombo;
  HoverIndex := -1;
  MaxDropItems := 8;
end;

procedure TLoxCombo.ItemsCleared;
begin
  inherited;
  HoverIndex := -1;
  DroppedDown := False;
end;

function TLoxCombo.PopupActive: Boolean;
begin
  Result := DroppedDown and Visible and Enabled;
end;

procedure TLoxCombo.CancelGesture;
begin
  inherited;
  DroppedDown := False;
end;

function TLoxCombo.ItemHeight: Integer;
begin
  Result := GuiTextHeight + 2;
end;

function TLoxCombo.VisibleItems: Integer;
begin
  Result := Items.Count;
  if Result > MaxDropItems then Result := MaxDropItems;
  if Result < 1 then Result := 1;  // an empty combo drops one blank row
end;

function TLoxCombo.DropdownRect: TRect;
var
  dropH: Integer;
begin
  dropH := VisibleItems * ItemHeight + 2;  // +2: 1px border top+bottom
  // Open downward by default; flip above the field when the canvas
  // bottom is too close (same as a real combo near the screen edge).
  if (Y + H + dropH > CanvasLogicalHeight) and (Y - dropH >= 0) then
    Result := Rect(X, Y - dropH, X + W, Y)
  else
    Result := Rect(X, Y + H, X + W, Y + H + dropH);
end;

function TLoxCombo.ItemAtPoint(PX, PY: Integer): Integer;
var
  DR: TRect;
  idx, rows: Integer;
begin
  Result := -1;
  DR := DropdownRect;
  if (PX < DR.Left + 1) or (PX >= DR.Right - 1) or
     (PY < DR.Top + 1) or (PY >= DR.Bottom - 1) then Exit;
  idx := (PY - (DR.Top + 1)) div ItemHeight;
  rows := Items.Count;
  if rows > VisibleItems then rows := VisibleItems;
  if (idx >= 0) and (idx < rows) then
    Result := idx;
end;

function TLoxCombo.HitTest(PX, PY: Integer): Boolean;
var
  DR: TRect;
begin
  Result := inherited HitTest(PX, PY);
  if (not Result) and DroppedDown then
  begin
    DR := DropdownRect;
    Result := (PX >= DR.Left) and (PX < DR.Right) and
              (PY >= DR.Top) and (PY < DR.Bottom);
  end;
end;

procedure TLoxCombo.HandleInput(const Frame: TFrameInput;
  var PointerClaimed: Boolean);
var
  overField, overDrop: Boolean;
  idx, i: Integer;
begin
  // The combo owns its whole input protocol (field + overlay), so it
  // does not run the base press/release gesture — ClickedEdge stays
  // unused for combos; selection is the meaningful event.
  if not Visible then
  begin
    CancelGesture;  // also folds the dropdown
    Exit;
  end;

  overField := (not PointerClaimed) and
               (Frame.MouseX >= X) and (Frame.MouseX < X + W) and
               (Frame.MouseY >= Y) and (Frame.MouseY < Y + H);
  overDrop := DroppedDown and (not PointerClaimed) and (not overField) and
              HitTest(Frame.MouseX, Frame.MouseY);
  if overField or overDrop then
    PointerClaimed := True;

  Hover := overField and Enabled;

  if not Enabled then
  begin
    CancelGesture;
    Exit;
  end;

  if Frame.LeftPressedEdge then
  begin
    if overField then
    begin
      // Toggle the dropdown; opening highlights the current selection.
      DroppedDown := not DroppedDown;
      if DroppedDown then
        HoverIndex := ItemIndex;
      FFocusedId := Id;
    end
    else if overDrop then
    begin
      // Press-selects (a real combo tracks to release; press-select
      // keeps the poll-based engine simple and feels fine at 60fps).
      idx := ItemAtPoint(Frame.MouseX, Frame.MouseY);
      if idx >= 0 then
        SelectIndex(idx);
      DroppedDown := False;
    end
    else if DroppedDown then
      // Press anywhere else folds the dropdown; the engine's exclusive
      // routing already swallowed the press for everyone underneath.
      DroppedDown := False;
  end;

  // Track the highlight under the pointer while open.
  if DroppedDown and overDrop then
  begin
    idx := ItemAtPoint(Frame.MouseX, Frame.MouseY);
    if idx >= 0 then
      HoverIndex := idx;
  end;

  // The pushed arrow visual while dropped.
  Pressed := DroppedDown;

  // Keyboard, focused only.
  if not IsFocused then Exit;

  if DroppedDown then
  begin
    if Frame.NavUp and (HoverIndex > 0) then Dec(HoverIndex);
    if Frame.NavDown and (HoverIndex < VisibleItems - 1) and
       (HoverIndex < Items.Count - 1) then Inc(HoverIndex);
    for i := 1 to Length(Frame.Chars) do
      case Frame.Chars[i] of
        #13:  // Enter commits the highlighted row
          begin
            SelectIndex(HoverIndex);
            DroppedDown := False;
          end;
        #27:  // Escape folds without selecting
          DroppedDown := False;
      end;
  end
  else
  begin
    // Closed + focused: up/down step the selection directly, like a
    // csDropDownList TComboBox.
    if Frame.NavUp and (ItemIndex > 0) then
      SelectIndex(ItemIndex - 1);
    if Frame.NavDown and (ItemIndex < Items.Count - 1) then
      SelectIndex(ItemIndex + 1);
  end;
end;

procedure TLoxCombo.Draw(Bm: TBitmap);
var
  R, Inner, ArrowR, TextR, DR, RowR: TRect;
  DC: HDC;
  OldFont: HGDIOBJ;
  SaveMode: Integer;
  SaveColor: TColorRef;
  State: UINT;
  arrowW, textY, i, rows: Integer;
  s: string;
begin
  R := Rect(X, Y, X + W, Y + H);
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;
  DC := Bm.Canvas.Handle;

  // Field: same sunken frame + fill as the edit box.
  Inner := R;
  DrawEdge(DC, Inner, EDGE_SUNKEN, BF_RECT or BF_ADJUST);
  if Enabled then
    FillRect(DC, Inner, GetSysColorBrush(COLOR_WINDOW))
  else
    FillRect(DC, Inner, GetSysColorBrush(COLOR_BTNFACE));

  // Classic combo arrow on the right edge of the client area.
  arrowW := GetSystemMetrics(SM_CXVSCROLL);
  if arrowW > Inner.Right - Inner.Left then
    arrowW := Inner.Right - Inner.Left;
  ArrowR := Rect(Inner.Right - arrowW, Inner.Top, Inner.Right, Inner.Bottom);
  State := DFCS_SCROLLCOMBOBOX;
  if DroppedDown then
    State := State or DFCS_PUSHED;
  if not Enabled then
    State := State or DFCS_INACTIVE;
  DrawFrameControl(DC, ArrowR, DFC_SCROLL, State);

  OldFont := SelectObject(DC, GetStockObject(DEFAULT_GUI_FONT));
  SaveMode := SetBkMode(DC, TRANSPARENT);

  // Selected text, with the classic focused-field inversion.
  TextR := Rect(Inner.Left, Inner.Top, ArrowR.Left, Inner.Bottom);
  if IsFocused and Enabled and not DroppedDown then
  begin
    FillRect(DC, TextR, GetSysColorBrush(COLOR_HIGHLIGHT));
    SaveColor := SetTextColor(DC, GetSysColor(COLOR_HIGHLIGHTTEXT));
  end
  else if Enabled then
    SaveColor := SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT))
  else
    SaveColor := SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));

  if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
  begin
    s := Items[ItemIndex];
    textY := TextR.Top + (TextR.Bottom - TextR.Top - GuiTextHeight) div 2;
    ExtTextOutW(DC, TextR.Left + EditPad, textY, ETO_CLIPPED, @TextR,
      PWideChar(s), Length(s), nil);
  end;

  // Dropdown overlay: 1px window-frame border, white interior, rows
  // with a highlight bar. WidgetsDrawAll draws us last while open, so
  // this legitimately paints over neighbouring widgets.
  if DroppedDown then
  begin
    DR := DropdownRect;
    FillRect(DC, DR, GetSysColorBrush(COLOR_WINDOWFRAME));
    RowR := Rect(DR.Left + 1, DR.Top + 1, DR.Right - 1, DR.Bottom - 1);
    FillRect(DC, RowR, GetSysColorBrush(COLOR_WINDOW));

    rows := Items.Count;
    if rows > VisibleItems then rows := VisibleItems;
    for i := 0 to rows - 1 do
    begin
      RowR := Rect(DR.Left + 1, DR.Top + 1 + i * ItemHeight,
                   DR.Right - 1, DR.Top + 1 + (i + 1) * ItemHeight);
      if i = HoverIndex then
      begin
        FillRect(DC, RowR, GetSysColorBrush(COLOR_HIGHLIGHT));
        SetTextColor(DC, GetSysColor(COLOR_HIGHLIGHTTEXT));
      end
      else
        SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT));
      s := Items[i];
      ExtTextOutW(DC, RowR.Left + EditPad, RowR.Top + 1, ETO_CLIPPED, @RowR,
        PWideChar(s), Length(s), nil);
    end;
  end;

  SetTextColor(DC, SaveColor);
  SetBkMode(DC, SaveMode);
  SelectObject(DC, OldFont);
end;

// Classic dotted focus box with deterministic pixels (see the comment
// at its call site for why DrawFocusRect's XOR is avoided). Uses a
// cosmetic PS_ALTERNATE pen: 1px, every other pixel, button-text
// colour, hollow interior.
procedure DrawDottedRect(DC: HDC; const R: TRect);
var
  lb: TLogBrush;
  pen: HPEN;
  oldPen, oldBrush: HGDIOBJ;
begin
  lb.lbStyle := BS_SOLID;
  lb.lbColor := GetSysColor(COLOR_BTNTEXT);
  lb.lbHatch := 0;
  pen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE, 1, lb, 0, nil);
  if pen = 0 then Exit;
  oldPen := SelectObject(DC, pen);
  oldBrush := SelectObject(DC, GetStockObject(NULL_BRUSH));
  Rectangle(DC, R.Left, R.Top, R.Right, R.Bottom);
  SelectObject(DC, oldBrush);
  SelectObject(DC, oldPen);
  DeleteObject(pen);
end;

{ TLoxRadioGroup }

const
  RadioGlyph = 13;       // classic radio circle is 13x13
  RadioIndent = 8;       // items column inset from the group's left
  RadioLabelGap = 5;     // gap between glyph and label

constructor TLoxRadioGroup.Create(AId: Integer);
begin
  inherited Create(AId);
  Kind := wkRadioGroup;
end;

function TLoxRadioGroup.RowHeight: Integer;
begin
  Result := GuiTextHeight + 4;
end;

function TLoxRadioGroup.ItemsTop: Integer;
begin
  // Items start below the caption strip sitting on the frame's top
  // edge (present even when Caption is '', like TRadioGroup).
  Result := Y + GuiTextHeight + 2;
end;

function TLoxRadioGroup.ItemAtPoint(PX, PY: Integer): Integer;
var
  idx: Integer;
begin
  Result := -1;
  if (PX < X + RadioIndent) or (PX >= X + W - 2) then Exit;
  if PY < ItemsTop then Exit;
  idx := (PY - ItemsTop) div RowHeight;
  // Rows clipped by the group's height are not clickable either.
  if (idx >= 0) and (idx < Items.Count) and
     (ItemsTop + (idx + 1) * RowHeight <= Y + H - 2) then
    Result := idx;
end;

procedure TLoxRadioGroup.HandleInput(const Frame: TFrameInput;
  var PointerClaimed: Boolean);
var
  idx: Integer;
begin
  // The base runs the standard gesture (claim, focus, capture,
  // release-inside click) over the whole group rect.
  inherited HandleInput(Frame, PointerClaimed);

  // Convert a completed click into a selection: the item row under the
  // release point wins. The raw click edge is consumed here — a radio
  // group's meaningful event is the selection change, not the click.
  if ClickedEdge then
  begin
    ClickedEdge := False;
    idx := ItemAtPoint(Frame.MouseX, Frame.MouseY);
    if idx >= 0 then
      SelectIndex(idx);
  end;

  if not (IsFocused and Visible and Enabled) then Exit;
  if Frame.NavUp and (ItemIndex > 0) then
    SelectIndex(ItemIndex - 1);
  if Frame.NavDown and (ItemIndex < Items.Count - 1) then
    SelectIndex(ItemIndex + 1);
end;

procedure TLoxRadioGroup.Draw(Bm: TBitmap);
var
  R, FrameR, CapR, RowR, GlyphR, LabelR: TRect;
  DC: HDC;
  OldFont: HGDIOBJ;
  SaveMode: Integer;
  SaveColor: TColorRef;
  State: UINT;
  i, rowTop, glyphTop, capW: Integer;
  s: string;
begin
  R := Rect(X, Y, X + W, Y + H);
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;
  DC := Bm.Canvas.Handle;

  // The group is a solid BTNFACE panel (widgets assume the classic
  // dialog surface — labels must stay readable over any canvas art)
  // with an etched frame whose top edge runs through the caption
  // midline, exactly like TGroupBox/TRadioGroup.
  FrameR := Rect(X, Y + GuiTextHeight div 2, X + W, Y + H);
  FillRect(DC, R, GetSysColorBrush(COLOR_BTNFACE));
  DrawEdge(DC, FrameR, EDGE_ETCHED, BF_RECT);

  OldFont := SelectObject(DC, GetStockObject(DEFAULT_GUI_FONT));
  SaveMode := SetBkMode(DC, TRANSPARENT);
  if Enabled then
    SaveColor := SetTextColor(DC, GetSysColor(COLOR_BTNTEXT))
  else
    SaveColor := SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));

  if Caption <> '' then
  begin
    // Caption strip erases the frame line behind it (the classic gap).
    capW := MeasureGuiText(Caption);
    CapR := Rect(X + RadioIndent, Y, X + RadioIndent + capW + 4,
                 Y + GuiTextHeight);
    if CapR.Right > X + W - 2 then CapR.Right := X + W - 2;
    FillRect(DC, CapR, GetSysColorBrush(COLOR_BTNFACE));
    ExtTextOutW(DC, CapR.Left + 2, Y, ETO_CLIPPED, @CapR,
      PWideChar(Caption), Length(Caption), nil);
  end;

  for i := 0 to Items.Count - 1 do
  begin
    rowTop := ItemsTop + i * RowHeight;
    if rowTop + RowHeight > Y + H - 2 then Break;  // clipped rows: stop
    RowR := Rect(X + RadioIndent, rowTop, X + W - 2, rowTop + RowHeight);

    glyphTop := rowTop + (RowHeight - RadioGlyph) div 2;
    GlyphR := Rect(RowR.Left, glyphTop, RowR.Left + RadioGlyph,
                   glyphTop + RadioGlyph);
    State := DFCS_BUTTONRADIO;
    if i = ItemIndex then
      State := State or DFCS_CHECKED;
    if not Enabled then
      State := State or DFCS_INACTIVE;
    DrawFrameControl(DC, GlyphR, DFC_BUTTON, State);

    s := Items[i];
    LabelR := Rect(GlyphR.Right + RadioLabelGap, rowTop, RowR.Right,
                   rowTop + RowHeight);
    ExtTextOutW(DC, LabelR.Left + 1,
      rowTop + (RowHeight - GuiTextHeight) div 2, ETO_CLIPPED, @LabelR,
      PWideChar(s), Length(s), nil);

    // Focus feedback on the selected item's label, TRadioGroup-style.
    // Drawn as a real dotted rectangle rather than DrawFocusRect:
    // DrawFocusRect XORs the pixels underneath, which makes it the one
    // NON-idempotent draw in the engine — any replayed or reordered
    // GDI work cancels it visibly (it flickers) while every other
    // widget element survives unchanged. A PS_ALTERNATE pen gives the
    // same classic dotted box with deterministic pixels.
    if IsFocused and Enabled and (i = ItemIndex) then
    begin
      LabelR.Right := LabelR.Left + MeasureGuiText(s) + 3;
      if LabelR.Right > RowR.Right then LabelR.Right := RowR.Right;
      DrawDottedRect(DC, LabelR);
    end;
  end;

  SetTextColor(DC, SaveColor);
  SetBkMode(DC, SaveMode);
  SelectObject(DC, OldFont);
end;

// -- Widget list management -------------------------------------------

procedure EnsureList;
begin
  if FWidgets = nil then
    FWidgets := TObjectList<TLoxWidget>.Create(True {OwnsObjects});
end;

// Linear ID -> widget lookup. Widget counts in a script are tiny
// (buttons on a form, not per-pixel), so this is fine and avoids
// dragging in a dictionary + its rehash cost during Clear.
function FindWidget(Id: Integer): TLoxWidget;
var
  i: Integer;
begin
  Result := nil;
  if (FWidgets = nil) or (Id <= 0) then Exit;
  for i := 0 to FWidgets.Count - 1 do
    if (FWidgets[i] <> nil) and (FWidgets[i].Id = Id) then
    begin
      Result := FWidgets[i];
      Exit;
    end;
end;

function WidgetsCreateButton(X, Y, W, H: Integer;
  const Caption: string): Integer;
var
  Btn: TLoxButton;
begin
  EnsureList;
  Btn := TLoxButton.Create(FNextId);
  Inc(FNextId);
  Btn.X := X;
  Btn.Y := Y;
  Btn.W := W;
  Btn.H := H;
  Btn.Caption := Caption;
  FWidgets.Add(Btn);
  Result := Btn.Id;
end;

function WidgetsCreateEdit(X, Y, W, H: Integer;
  const Text: string): Integer;
var
  Ed: TLoxEdit;
begin
  EnsureList;
  Ed := TLoxEdit.Create(FNextId);
  Inc(FNextId);
  Ed.X := X;
  Ed.Y := Y;
  Ed.W := W;
  Ed.H := H;
  Ed.Text := Text;
  Ed.CaretPos := Length(Text);
  FWidgets.Add(Ed);
  Result := Ed.Id;
end;

// Typed lookup for the edit accessors: nil when the id is unknown OR
// names a widget of another kind.
function FindEdit(Id: Integer): TLoxEdit;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  if Wd is TLoxEdit then
    Result := TLoxEdit(Wd)
  else
    Result := nil;
end;

procedure WidgetsSetText(Id: Integer; const Text: string);
var
  Ed: TLoxEdit;
begin
  Ed := FindEdit(Id);
  if Ed = nil then Exit;
  Ed.Text := Text;
  Ed.CaretPos := Length(Text);
  Ed.EnsureCaretVisible;
  // Deliberately no ChangedEdge: a script setting the text shouldn't
  // then observe its own change (avoids poll/handler feedback loops).
end;

function WidgetsGetText(Id: Integer): string;
var
  Ed: TLoxEdit;
begin
  Ed := FindEdit(Id);
  if Ed = nil then Exit('');
  Result := Ed.Text;
end;

procedure WidgetsSetMaxLength(Id, MaxLen: Integer);
var
  Ed: TLoxEdit;
begin
  Ed := FindEdit(Id);
  if Ed = nil then Exit;
  if MaxLen < 0 then MaxLen := 0;
  Ed.MaxLength := MaxLen;
  // Enforce immediately, like TEdit: existing overflow is truncated.
  if (MaxLen > 0) and (Length(Ed.Text) > MaxLen) then
  begin
    Ed.Text := Copy(Ed.Text, 1, MaxLen);
    if Ed.CaretPos > MaxLen then Ed.CaretPos := MaxLen;
    Ed.EnsureCaretVisible;
  end;
end;

function WidgetsConsumeChanged(Id: Integer): Boolean;
var
  Wd: TLoxWidget;
begin
  // ChangedEdge is base-class state: edits set it on text mutation,
  // combos on selection change — one consume works for both.
  Wd := FindWidget(Id);
  if Wd = nil then Exit(False);
  Result := Wd.ChangedEdge;
  Wd.ChangedEdge := False;
end;

function WidgetsConsumeSubmitted(Id: Integer): Boolean;
var
  Ed: TLoxEdit;
begin
  Ed := FindEdit(Id);
  if Ed = nil then Exit(False);
  Result := Ed.SubmittedEdge;
  Ed.SubmittedEdge := False;
end;

// Typed lookup for the item-list accessors, mirroring FindEdit.
function FindListWidget(Id: Integer): TLoxListWidget;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  if Wd is TLoxListWidget then
    Result := TLoxListWidget(Wd)
  else
    Result := nil;
end;

function WidgetsCreateCombo(X, Y, W, H: Integer): Integer;
var
  Cb: TLoxCombo;
begin
  EnsureList;
  Cb := TLoxCombo.Create(FNextId);
  Inc(FNextId);
  Cb.X := X;
  Cb.Y := Y;
  Cb.W := W;
  Cb.H := H;
  FWidgets.Add(Cb);
  Result := Cb.Id;
end;

function WidgetsCreateRadioGroup(X, Y, W, H: Integer;
  const Caption: string): Integer;
var
  Rg: TLoxRadioGroup;
begin
  EnsureList;
  Rg := TLoxRadioGroup.Create(FNextId);
  Inc(FNextId);
  Rg.X := X;
  Rg.Y := Y;
  Rg.W := W;
  Rg.H := H;
  Rg.Caption := Caption;
  FWidgets.Add(Rg);
  Result := Rg.Id;
end;

function WidgetsListAddItem(Id: Integer; const Text: string): Integer;
var
  Lw: TLoxListWidget;
begin
  Lw := FindListWidget(Id);
  if Lw = nil then Exit(-1);
  Result := Lw.Items.Add(Text);
end;

procedure WidgetsListClearItems(Id: Integer);
var
  Lw: TLoxListWidget;
begin
  Lw := FindListWidget(Id);
  if Lw = nil then Exit;
  Lw.Items.Clear;
  Lw.ItemIndex := -1;
  Lw.ItemsCleared;
end;

function WidgetsListItemCount(Id: Integer): Integer;
var
  Lw: TLoxListWidget;
begin
  Lw := FindListWidget(Id);
  if Lw = nil then Exit(0);
  Result := Lw.Items.Count;
end;

function WidgetsGetListIndex(Id: Integer): Integer;
var
  Lw: TLoxListWidget;
begin
  Lw := FindListWidget(Id);
  if Lw = nil then Exit(-1);
  Result := Lw.ItemIndex;
end;

procedure WidgetsSetListIndex(Id, Index: Integer);
var
  Lw: TLoxListWidget;
begin
  Lw := FindListWidget(Id);
  if Lw = nil then Exit;
  if Index < -1 then Index := -1;
  if Index >= Lw.Items.Count then Index := Lw.Items.Count - 1;
  Lw.ItemIndex := Index;
  // Deliberately no ChangedEdge — same contract as WidgetsSetText.
end;

function WidgetsGetListText(Id: Integer): string;
var
  Lw: TLoxListWidget;
begin
  Lw := FindListWidget(Id);
  if (Lw = nil) or (Lw.ItemIndex < 0) or
     (Lw.ItemIndex >= Lw.Items.Count) then Exit('');
  Result := Lw.Items[Lw.ItemIndex];
end;

function WidgetsGetFocus: Integer;
begin
  Result := FFocusedId;
end;

procedure WidgetsSetFocus(Id: Integer);
var
  Wd: TLoxWidget;
begin
  if Id = 0 then
  begin
    FFocusedId := 0;
    Exit;
  end;
  Wd := FindWidget(Id);
  if (Wd <> nil) and Wd.WantsFocus and Wd.Enabled and Wd.Visible then
    FFocusedId := Id;
end;

function WidgetsIsKind(Id: Integer; Kind: TWidgetKind): Boolean;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  Result := (Wd <> nil) and (Wd.Kind = Kind);
end;

procedure WidgetsSetBounds(Id, X, Y, W, H: Integer);
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  if Wd = nil then Exit;
  Wd.X := X;
  Wd.Y := Y;
  Wd.W := W;
  Wd.H := H;
end;

procedure WidgetsSetCaption(Id: Integer; const Caption: string);
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  if Wd = nil then Exit;
  Wd.Caption := Caption;
end;

procedure WidgetsSetEnabled(Id: Integer; Enabled: Boolean);
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  if Wd = nil then Exit;
  Wd.Enabled := Enabled;
  // Disabling a widget mid-gesture drops its capture so the pending
  // click never fires. Matches VCL: disabling a TButton mid-drag
  // never fires OnClick. Focus can't stay on a disabled widget either.
  if not Enabled then
  begin
    Wd.CancelGesture;
    if FFocusedId = Id then FFocusedId := 0;
  end;
end;

procedure WidgetsSetVisible(Id: Integer; Visible: Boolean);
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  if Wd = nil then Exit;
  Wd.Visible := Visible;
  // Same as disable: hiding drops any in-flight gesture and focus.
  if not Visible then
  begin
    Wd.CancelGesture;
    if FFocusedId = Id then FFocusedId := 0;
  end;
end;

function WidgetsExists(Id: Integer): Boolean;
begin
  Result := FindWidget(Id) <> nil;
end;

function WidgetsCount: Integer;
var
  i, n: Integer;
begin
  n := 0;
  if FWidgets <> nil then
    for i := 0 to FWidgets.Count - 1 do
      if FWidgets[i] <> nil then Inc(n);
  Result := n;
end;

procedure WidgetsFree(Id: Integer);
var
  i: Integer;
begin
  if FWidgets = nil then Exit;
  for i := 0 to FWidgets.Count - 1 do
    if (FWidgets[i] <> nil) and (FWidgets[i].Id = Id) then
    begin
      // Setting a slot to nil in an OwnsObjects list frees the object
      // (see TObjectList.Notify). We keep the slot so the linear
      // FindWidget scan stays stable — count-based indices could go
      // stale mid-frame otherwise.
      FWidgets[i] := nil;
      if FFocusedId = Id then FFocusedId := 0;
      Exit;
    end;
end;

procedure WidgetsClear;
begin
  if FWidgets <> nil then
    FWidgets.Clear;
  FNextId := 1;
  FPrevLeftDown := False;
  FFocusedId := 0;
end;

// -- Per-frame input and rendering -------------------------------------

procedure WidgetsUpdate;
var
  i: Integer;
  Frame: TFrameInput;
  Claimed: Boolean;
  Capturer, Popup: TLoxWidget;
begin
  if FWidgets = nil then
  begin
    FPrevLeftDown := False;
    Exit;
  end;

  Frame.MouseX := CanvasMouseLogicalX;
  Frame.MouseY := CanvasMouseLogicalY;
  Frame.LeftDown := CanvasMouseButtonHeld(0);
  // CanvasMouseButtonClickedEdge is the down-edge from the input
  // pump (cleared each processMessages tick). It gives us the
  // pressed-this-frame signal without needing our own edge tracker.
  Frame.LeftPressedEdge := CanvasMouseButtonClickedEdge(0);
  // Release edge: derived here because the canvas doesn't expose one.
  // We compare the previous frame's held state to this frame's.
  Frame.LeftReleasedEdge := FPrevLeftDown and not Frame.LeftDown;
  // Keyboard for the focused widget: this frame's WM_CHAR stream and
  // the edge-triggered navigation keys.
  Frame.Chars := CanvasTypedChars;
  Frame.NavLeft := CanvasKeyPressedEdge('left');
  Frame.NavRight := CanvasKeyPressedEdge('right');
  Frame.NavHome := CanvasKeyPressedEdge('home');
  Frame.NavEnd := CanvasKeyPressedEdge('end');
  Frame.NavDelete := CanvasKeyPressedEdge('delete');
  Frame.NavUp := CanvasKeyPressedEdge('up');
  Frame.NavDown := CanvasKeyPressedEdge('down');

  // Windows-style mouse capture: while a gesture is in flight, its
  // owner receives the pointer exclusively — nothing else hovers or
  // starts a capture until the button is released.
  Capturer := nil;
  for i := 0 to FWidgets.Count - 1 do
    if (FWidgets[i] <> nil) and FWidgets[i].Capturing then
    begin
      Capturer := FWidgets[i];
      Break;
    end;

  // An open popup (dropped combo) gets the same exclusivity as a
  // capture: it sees the pointer first and everyone else sees it as
  // elsewhere, so a press outside the popup folds it without also
  // activating whatever sat underneath.
  Popup := nil;
  if Capturer = nil then
    for i := FWidgets.Count - 1 downto 0 do
      if (FWidgets[i] <> nil) and FWidgets[i].PopupActive then
      begin
        Popup := FWidgets[i];
        Break;
      end;

  Claimed := False;
  if Capturer <> nil then
  begin
    Capturer.HandleInput(Frame, Claimed);
    Claimed := True;  // capture is exclusive regardless of pointer position
  end
  else if Popup <> nil then
  begin
    Popup.HandleInput(Frame, Claimed);
    Claimed := True;  // popup is modal for the pointer while open
  end;

  // Everyone else, topmost (last created) first — the first widget hit
  // claims the pointer, so overlaps resolve to the one drawn on top.
  for i := FWidgets.Count - 1 downto 0 do
    if (FWidgets[i] <> nil) and (FWidgets[i] <> Capturer) and
       (FWidgets[i] <> Popup) then
      FWidgets[i].HandleInput(Frame, Claimed);

  // A press that no widget claimed landed on empty canvas — clear
  // keyboard focus, like clicking a form's background in VCL.
  if Frame.LeftPressedEdge and not Claimed then
    FFocusedId := 0;

  FPrevLeftDown := Frame.LeftDown;
end;

procedure WidgetsDrawAll;
var
  i: Integer;
  Bm: TBitmap;
  Wd, Popup: TLoxWidget;
begin
  if (FWidgets = nil) or (FWidgets.Count = 0) then Exit;

  // Force the back buffer into existence so scripts can call
  // drawWidgets() before any other draw native has run this frame
  // (e.g. a menu screen that only shows buttons on a clearCanvas
  // background). Also rebinds FRenderTarget onto the back buffer if
  // the last surface got yanked.
  Bm := CanvasEnsureRenderTarget;
  if Bm = nil then Exit;

  // A popup-active widget (open dropdown) is deferred and drawn last
  // so its overlay sits above every other widget.
  Popup := nil;
  for i := 0 to FWidgets.Count - 1 do
  begin
    Wd := FWidgets[i];
    if (Wd = nil) or (not Wd.Visible) then Continue;
    if (Popup = nil) and Wd.PopupActive then
    begin
      Popup := Wd;
      Continue;
    end;
    Wd.Draw(Bm);
  end;
  if Popup <> nil then
    Popup.Draw(Bm);

  // Drain the thread's GDI batch before anyone touches the DIB again.
  // Widget drawing is the only GDI writer on the buffer — everything
  // else (fillRect, drawText, present's buffer clear) writes the DIB
  // bits directly, and MSDN requires a GdiFlush between queued GDI ops
  // and direct bit access on a DIB section. Without it the batched
  // widget ops can execute out of order against the rasterizer's
  // writes (widgets flicker or lag a frame behind).
  GdiFlush;
end;

// -- State queries ------------------------------------------------------

function WidgetsConsumeClick(Id: Integer): Boolean;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  if Wd = nil then Exit(False);
  Result := Wd.ClickedEdge;
  Wd.ClickedEdge := False;
end;

function WidgetsIsClicked(Id: Integer): Boolean;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  Result := (Wd <> nil) and Wd.ClickedEdge;
end;

function WidgetsIsHover(Id: Integer): Boolean;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  Result := (Wd <> nil) and Wd.Hover;
end;

function WidgetsIsPressed(Id: Integer): Boolean;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  Result := (Wd <> nil) and Wd.Pressed;
end;

function WidgetsIsEnabled(Id: Integer): Boolean;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  Result := (Wd <> nil) and Wd.Enabled;
end;

function WidgetsIsVisible(Id: Integer): Boolean;
var
  Wd: TLoxWidget;
begin
  Wd := FindWidget(Id);
  Result := (Wd <> nil) and Wd.Visible;
end;

initialization

finalization
  FreeAndNil(FWidgets);
  if FMeasureDC <> 0 then
  begin
    DeleteDC(FMeasureDC);
    FMeasureDC := 0;
  end;

end.
