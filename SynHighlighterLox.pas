unit SynHighlighterLox;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics,
  SynEditHighlighter, SynEditTypes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKeyword, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkBuiltIn);

  TSynLoxSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FBuiltInAttri: TSynHighlighterAttributes;
    procedure IdentProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure NullProc;
    procedure UnknownProc;
    function IsKeyword(const AToken: string): Boolean;
    function IsBuiltIn(const AToken: string): Boolean;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property BuiltInAttri: TSynHighlighterAttributes read FBuiltInAttri write FBuiltInAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  LoxKeywords: array[0..18] of string = (
    'and', 'class', 'else', 'false', 'for', 'fun', 'if', 'nil', 'or',
    'print', 'record', 'return', 'super', 'this', 'true', 'var', 'while',
    'break', 'continue'
  );

  LoxBuiltIns: array[0..54] of string = (
    // Core
    'clock', 'collectGarbage', 'assert',
    // Conversion
    'str', 'num', 'bool', 'type',
    // VM introspection
    'bytesAllocated', 'objectsAllocated', 'vmStackDepth', 'vmStackCapacity',
    'vmCallDepth', 'vmOpenUpvalues', 'gcNextThreshold', 'gcCollectionCount',
    'internTableStats',
    // Environment
    'env', 'loadEnv',
    // Math
    'abs', 'ceil', 'floor', 'round', 'sqrt', 'pow', 'min', 'max', 'random',
    // String
    'strlen', 'substr', 'indexOf', 'charAt', 'upper', 'lower', 'trim', 'split',
    // Array
    'newArray', 'arrayPush', 'arrayPop', 'arrayGet', 'arraySet', 'arrayLen', 'arrayRemove',
    // Dictionary
    'dictNew', 'dictSet', 'dictGet', 'dictHas', 'dictDelete', 'dictKeys', 'dictSize', 'dictValues',
    // Native objects
    'StringList',
    // SQL
    'sqlConnect', 'sqlQuery', 'sqlQueryParams', 'sqlClose'
  );

{ TSynLoxSyn }

constructor TSynLoxSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommentAttri := TSynHighlighterAttributes.Create('Comment', 'Comment');
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create('Identifier', 'Identifier');
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighlighterAttributes.Create('Keyword', 'Keyword');
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clNavy;
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create('Number', 'Number');
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create('Space', 'Space');
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create('String', 'String');
  FStringAttri.Foreground := clMaroon;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create('Symbol', 'Symbol');
  AddAttribute(FSymbolAttri);

  FBuiltInAttri := TSynHighlighterAttributes.Create('BuiltIn', 'BuiltIn');
  FBuiltInAttri.Foreground := clTeal;
  AddAttribute(FBuiltInAttri);

  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := 'Lox files (*.lox)|*.lox';
end;

function TSynLoxSyn.IsKeyword(const AToken: string): Boolean;
var
  i: Integer;
begin
  for i := Low(LoxKeywords) to High(LoxKeywords) do
    if SameText(AToken, LoxKeywords[i]) then
      Exit(True);
  Result := False;
end;

function TSynLoxSyn.IsBuiltIn(const AToken: string): Boolean;
var
  i: Integer;
begin
  for i := Low(LoxBuiltIns) to High(LoxBuiltIns) do
    if SameText(AToken, LoxBuiltIns[i]) then
      Exit(True);
  Result := False;
end;

procedure TSynLoxSyn.IdentProc;
var
  Start: Integer;
  Token: string;
begin
  Start := Run;
  while (Run < fLineLen) and CharInSet(fLine[Run], ['a'..'z','A'..'Z','0'..'9','_']) do
    Inc(Run);
  SetString(Token, fLine + Start, Run - Start);
  if IsKeyword(Token) then
    FTokenID := tkKeyword
  else if IsBuiltIn(Token) then
    FTokenID := tkBuiltIn
  else
    FTokenID := tkIdentifier;
end;

procedure TSynLoxSyn.NumberProc;
begin
  FTokenID := tkNumber;
  while (Run < fLineLen) and CharInSet(fLine[Run], ['0'..'9']) do
    Inc(Run);
  if (Run < fLineLen) and (fLine[Run] = '.') then
  begin
    Inc(Run);
    while (Run < fLineLen) and CharInSet(fLine[Run], ['0'..'9']) do
      Inc(Run);
  end;
end;

procedure TSynLoxSyn.SlashProc;
begin
  Inc(Run);
  if (Run < fLineLen) and (fLine[Run] = '/') then
  begin
    // Line comment
    FTokenID := tkComment;
    Run := fLineLen;
  end
  else
  begin
    FTokenID := tkSymbol;
  end;
end;

procedure TSynLoxSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  while (Run < fLineLen) and (fLine[Run] <= ' ') do
    Inc(Run);
end;

procedure TSynLoxSyn.StringProc;
var
  Quote: Char;
begin
  FTokenID := tkString;
  Quote := fLine[Run];
  Inc(Run);
  while Run < fLineLen do
  begin
    if fLine[Run] = Quote then
    begin
      Inc(Run);
      Break;
    end;
    if fLine[Run] = '\' then
      Inc(Run);
    Inc(Run);
  end;
end;

procedure TSynLoxSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  // Handle two-char operators: !=, ==, <=, >=
  if (Run < fLineLen) then
  begin
    case fLine[Run - 1] of
      '!', '=', '<', '>':
        if fLine[Run] = '=' then Inc(Run);
    end;
  end;
end;

procedure TSynLoxSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynLoxSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynLoxSyn.Next;
begin
  fTokenPos := Run;
  if Run >= fLineLen then
    NullProc
  else
    case fLine[Run] of
      'a'..'z', 'A'..'Z', '_': IdentProc;
      '0'..'9': NumberProc;
      '/': SlashProc;
      '"': StringProc;
      #1..' ': SpaceProc;
      '+', '-', '*', '(', ')', '{', '}', '[', ']', ',', '.', ';',
      '!', '=', '<', '>', ':', '?', '%', '&', '|', '^', '~', '#', '@': SymbolProc;
    else
      UnknownProc;
    end;
  inherited;
end;

function TSynLoxSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynLoxSyn.GetEol: Boolean;
begin
  Result := Run > fLineLen;
end;

function TSynLoxSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynLoxSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKeyword: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkBuiltIn: Result := FBuiltInAttri;
  else
    Result := nil;
  end;
end;

function TSynLoxSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

class function TSynLoxSyn.GetLanguageName: string;
begin
  Result := 'Lox';
end;

class function TSynLoxSyn.GetFriendlyLanguageName: string;
begin
  Result := 'Lox Script';
end;

function TSynLoxSyn.GetSampleSource: string;
begin
  Result :=
    '// Hello world in Lox'#13#10 +
    'var greeting = "Hello, world!";'#13#10 +
    'print greeting;'#13#10 +
    ''#13#10 +
    'fun fibonacci(n) {'#13#10 +
    '  if (n <= 1) return n;'#13#10 +
    '  return fibonacci(n - 1) + fibonacci(n - 2);'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'for (var i = 0; i < 10; i = i + 1) {'#13#10 +
    '  print fibonacci(i);'#13#10 +
    '}';
end;

function TSynLoxSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> 'Lox files (*.lox)|*.lox';
end;

end.
