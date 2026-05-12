unit NativeObjects;

interface

uses
  Chunk_Types, Generics.Collections;

type
  // ---- Queue for event passing between Delphi and Lox ----
  TLoxQueue = class
  private
    FItems: TQueue<string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Value: string);
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
  FItems := TQueue<string>.Create;
end;

destructor TLoxQueue.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLoxQueue.Enqueue(const Value: string);
begin
  FItems.Enqueue(Value);
end;

function TLoxQueue.dequeue: string;
begin
  if FItems.Count = 0 then
    Exit('');
  Result := FItems.Dequeue;
end;

function TLoxQueue.peek: string;
begin
  if FItems.Count = 0 then
    Exit('');
  Result := FItems.Peek;
end;

function TLoxQueue.count: Integer;
begin
  Result := FItems.Count;
end;

function TLoxQueue.hasItems: Boolean;
begin
  Result := FItems.Count > 0;
end;

procedure TLoxQueue.clear;
begin
  FItems.Clear;
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
  FAddress.Free;
  inherited;
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
    FAddress.Free;
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
