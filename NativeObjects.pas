unit NativeObjects;

interface

uses
  Classes, Suto, Generics.Collections;

type
  // ---- Queue for event passing between Delphi and Lox ----
  // Backed by TList<string> + a head index instead of TQueue<string> so
  // we can peek/replace the tail element. The tail-replace path is used
  // by mouse-move coalescing: consecutive mousemove events overwrite
  // each other in place rather than piling up in the queue.
  [LoxClass('LoxQueue')]
  TLoxQueue = class
  private
    FItems: TList<string>;
    FHead: Integer;
    procedure Compact;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Value: string);
    // Returns True and overwrites the most-recently-enqueued item if
    // it begins with Prefix; otherwise returns False without modifying
    // the queue. Used by the event-dispatch layer for coalescing.
    function ReplaceTailIfPrefix(const Prefix, NewValue: string): Boolean;
    [LoxMethod]
    function dequeue: string;
    [LoxMethod]
    function peek: string;
    [LoxMethod]
    function count: Integer;
    [LoxMethod]
    function hasItems: Boolean;
    [LoxMethod]
    procedure clear;
  end;

  // Enum type for RTTI marshaling tests
  TCustomerTier = (ctBronze, ctSilver, ctGold, ctPlatinum);

  // Set type for RTTI marshaling tests
  TCustomerFlag = (cfNewsletter, cfVIP, cfTaxExempt, cfWholesale);
  TCustomerFlags = set of TCustomerFlag;

  [LoxClass('Address')]
  TAddress = class
  private
    FStreet: string;
    FCity: string;
    FZip: string;
  public
    constructor Create(const AStreet, ACity, AZip: string);
    [LoxProperty]
    property Street: string read FStreet write FStreet;
    [LoxProperty]
    property City: string read FCity write FCity;
    [LoxProperty]
    property Zip: string read FZip write FZip;
  end;

  [LoxClass('Customer')]
  TCustomer = class
  private
    FName: string;
    FAge: Integer;
    FBalance: Double;
    FActive: Boolean;
    FAddress: TAddress;
    FTier: TCustomerTier;
    FFlags: TCustomerFlags;
    FTags: TArray<string>;
    FOnChanged: TNotifyEvent;
  public
    constructor Create(const AName: string; AAge: Integer; ABalance: Double; AActive: Boolean);
    destructor Destroy; override;
    [LoxProperty]
    property Name: string read FName write FName;
    [LoxProperty]
    property Age: Integer read FAge write FAge;
    [LoxProperty]
    property Balance: Double read FBalance write FBalance;
    [LoxProperty]
    property Active: Boolean read FActive write FActive;
    [LoxProperty]
    property Address: TAddress read FAddress write FAddress;
    [LoxProperty]
    property Tier: TCustomerTier read FTier write FTier;
    [LoxProperty]
    property Flags: TCustomerFlags read FFlags write FFlags;
    [LoxProperty]
    property Tags: TArray<string> read FTags write FTags;
    // Event property: scripts can assign a Lox function here (marshaled to a
    // TNotifyEvent via TLoxCallbackAdapter) and Touch fires it.
    [LoxProperty]
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    [LoxMethod]
    procedure Touch;
    [LoxMethod]
    function Greet: string;
    [LoxMethod]
    procedure AddBalance(Amount: Double);
    [LoxMethod]
    procedure SetInfo(const NewName: string; NewAge: Integer);
    [LoxMethod]
    function Describe(const Prefix: string): string;
    [LoxMethod]
    procedure SetAddress(NewAddr: TAddress);
    [LoxMethod]
    function GetTags: TArray<string>;
    [LoxMethod]
    procedure SetTags(const NewTags: TArray<string>);
  end;

implementation

uses
  SysUtils;

{ TLoxQueue }

constructor TLoxQueue.Create;
begin
  inherited Create;
  FItems := TList<string>.Create;
  FHead := 0;
end;

destructor TLoxQueue.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLoxQueue.Compact;
begin
  // Shift live items down to the front and reset FHead. Called when
  // the dead-prefix region is large enough that O(n) memmove is worth
  // it to keep the backing TArray from growing without bound on
  // long-lived queues (e.g. UI session with thousands of events).
  if FHead > 0 then
  begin
    FItems.DeleteRange(0, FHead);
    FHead := 0;
  end;
end;

procedure TLoxQueue.Enqueue(const Value: string);
begin
  FItems.Add(Value);
end;

function TLoxQueue.ReplaceTailIfPrefix(const Prefix, NewValue: string): Boolean;
var
  lastIdx: Integer;
begin
  Result := False;
  lastIdx := FItems.Count - 1;
  // Tail must be a live item (>= FHead). Empty/drained queue: nothing
  // to replace.
  if lastIdx < FHead then Exit;
  if Copy(FItems[lastIdx], 1, Length(Prefix)) = Prefix then
  begin
    FItems[lastIdx] := NewValue;
    Result := True;
  end;
end;

function TLoxQueue.dequeue: string;
begin
  if FHead >= FItems.Count then Exit('');
  Result := FItems[FHead];
  // Release the string ref-count immediately so a long-running queue
  // doesn't hold onto stale event strings until Compact runs.
  FItems[FHead] := '';
  Inc(FHead);
  // Compact when more than half the backing array is dead prefix and
  // there are at least a few items, to amortize the move cost.
  if (FHead >= 32) and (FHead * 2 >= FItems.Count) then
    Compact;
end;

function TLoxQueue.peek: string;
begin
  if FHead >= FItems.Count then Exit('');
  Result := FItems[FHead];
end;

function TLoxQueue.count: Integer;
begin
  Result := FItems.Count - FHead;
end;

function TLoxQueue.hasItems: Boolean;
begin
  Result := FHead < FItems.Count;
end;

procedure TLoxQueue.clear;
begin
  FItems.Clear;
  FHead := 0;
end;

{ TAddress }

constructor TAddress.Create(const AStreet, ACity, AZip: string);
begin
  inherited Create;
  FStreet := AStreet;
  FCity := ACity;
  FZip := AZip;
end;

{ TCustomer }

constructor TCustomer.Create(const AName: string; AAge: Integer; ABalance: Double; AActive: Boolean);
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
  FBalance := ABalance;
  FActive := AActive;
  FAddress := nil;
  FTier := ctBronze;
  FFlags := [];
  FTags := nil;
end;

destructor TCustomer.Destroy;
begin
  // Disarm any live script wrapper before freeing, so a Lox reference to the
  // old address errors cleanly instead of touching freed memory.
  ReleaseNativeInstance(FAddress);
  FAddress.Free;
  inherited;
end;

procedure TCustomer.Touch;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TCustomer.Greet: string;
begin
  Result := 'Hello, ' + FName + '!';
end;

procedure TCustomer.AddBalance(Amount: Double);
begin
  FBalance := FBalance + Amount;
end;

procedure TCustomer.SetInfo(const NewName: string; NewAge: Integer);
begin
  FName := NewName;
  FAge := NewAge;
end;

function TCustomer.Describe(const Prefix: string): string;
begin
  Result := Prefix + FName + ', age ' + IntToStr(FAge);
end;

procedure TCustomer.SetAddress(NewAddr: TAddress);
begin
  if FAddress <> NewAddr then
  begin
    // See Destroy: disarm the script wrapper before the instance dies.
    ReleaseNativeInstance(FAddress);
    FAddress.Free;
  end;
  FAddress := NewAddr;
end;

function TCustomer.GetTags: TArray<string>;
begin
  Result := FTags;
end;

procedure TCustomer.SetTags(const NewTags: TArray<string>);
begin
  FTags := NewTags;
end;

end.
