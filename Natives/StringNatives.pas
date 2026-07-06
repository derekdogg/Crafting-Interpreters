unit StringNatives;

interface

procedure RegisterStringNatives;

implementation

uses
  System.SysUtils, System.AnsiStrings, StrUtils, Suto, NativeRegistry;

// ---- String manipulation native functions ----

function strlenNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('strlen() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('strlen() argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateNumber(pObjString(GetObject(args[0]))^.length);
end;

function substrNative(argCount: integer; args: pValue): TValue;
var
  s : AnsiString;
  start, len, sLen : integer;
begin
  if argCount <> 3 then
  begin
    RuntimeError('substr() takes exactly 3 arguments (string, start, length).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('substr() first argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[1]) or not isNumber(args[2]) then
  begin
    RuntimeError('substr() start and length must be numbers.');
    Result := CreateNilValue;
    Exit;
  end;
  s := AsAnsiString(args[0]);
  sLen := Length(s);
  start := AsInteger(args[1]);
  len := AsInteger(args[2]);
  if (start < 0) or (start >= sLen) then
  begin
    RuntimeError('substr() start index out of bounds.');
    Result := CreateNilValue;
    Exit;
  end;
  if len < 0 then
  begin
    RuntimeError('substr() length must not be negative.');
    Result := CreateNilValue;
    Exit;
  end;
  // Clamp length to remaining characters
  if start + len > sLen then
    len := sLen - start;
  Result := CreateStringValue(Copy(s, start + 1, len));
end;

function indexOfNative(argCount: integer; args: pValue): TValue;
var
  haystack, needle : AnsiString;
  pos : integer;
begin
  if argCount <> 2 then
  begin
    RuntimeError('indexOf() takes exactly 2 arguments (string, needle).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) or not isString(args[1]) then
  begin
    RuntimeError('indexOf() arguments must be strings.');
    Result := CreateNilValue;
    Exit;
  end;
  haystack := AsAnsiString(args[0]);
  needle := AsAnsiString(args[1]);
  if Length(needle) = 0 then
    Result := CreateNumber(0)
  else
  begin
    pos := System.Pos(needle, haystack);
    if pos = 0 then
      Result := CreateNumber(-1)
    else
      Result := CreateNumber(pos - 1); // 0-based index
  end;
end;

function charAtNative(argCount: integer; args: pValue): TValue;
var
  s : AnsiString;
  idx : integer;
begin
  if argCount <> 2 then
  begin
    RuntimeError('charAt() takes exactly 2 arguments (string, index).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('charAt() first argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[1]) then
  begin
    RuntimeError('charAt() second argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  s := AsAnsiString(args[0]);
  idx := AsInteger(args[1]);
  if (idx < 0) or (idx >= Length(s)) then
  begin
    RuntimeError('charAt() index out of bounds.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateStringValue(AnsiString(s[idx + 1]));
end;

function upperNative(argCount: integer; args: pValue): TValue;
var
  s : AnsiString;
begin
  if argCount <> 1 then
  begin
    RuntimeError('upper() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('upper() argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  s := AsAnsiString(args[0]);
  Result := CreateStringValue(AnsiString(AnsiUpperCase(String(s))));
end;

function lowerNative(argCount: integer; args: pValue): TValue;
var
  s : AnsiString;
begin
  if argCount <> 1 then
  begin
    RuntimeError('lower() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('lower() argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  s := AsAnsiString(args[0]);
  Result := CreateStringValue(AnsiString(AnsiLowerCase(String(s))));
end;

function trimNative(argCount: integer; args: pValue): TValue;
var
  s : AnsiString;
begin
  if argCount <> 1 then
  begin
    RuntimeError('trim() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('trim() argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  s := AsAnsiString(args[0]);
  Result := CreateStringValue(AnsiString(Trim(String(s))));
end;

function splitNative(argCount: integer; args: pValue): TValue;
var
  s, delim, part : AnsiString;
  arr : pObjArray;
  objStr : pObjString;
  partVal : TValue;
  pos, start, delimLen, sLen : integer;
  newCap, oldSize, newSize : integer;

  procedure ArrayAppendStr(str : pObjString);
  var
    val : TValue;
  begin
    // Push string onto stack to protect from GC during potential array growth
    val := CreateObject(pObj(str));
    pushStack(VM.Stack, val);
    EnsureArrayCapacity(arr, VM.MemTracker);
    arr^.Elements[arr^.Count] := val;
    Inc(arr^.Count);
    Dec(VM.Stack.StackTop);
  end;

begin
  if argCount <> 2 then
  begin
    RuntimeError('split() takes exactly 2 arguments (string, delimiter).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isString(args[0]) or not isString(args[1]) then
  begin
    RuntimeError('split() arguments must be strings.');
    Result := CreateNilValue;
    Exit;
  end;
  s := AsAnsiString(args[0]);
  delim := AsAnsiString(args[1]);
  sLen := Length(s);
  delimLen := Length(delim);
  arr := newArray(VM.MemTracker);
  // Push array onto stack to protect from GC during string allocations
  pushStack(VM.Stack, CreateObject(pObj(arr)));

  if delimLen = 0 then
  begin
    // Split into individual characters
    for start := 1 to sLen do
    begin
      objStr := CreateString(AnsiString(s[start]), VM.MemTracker);
      ArrayAppendStr(objStr);
    end;
    Dec(VM.Stack.StackTop); // remove GC protection
    Result := CreateObject(pObj(arr));
    Exit;
  end;

  // Empty string with non-empty delimiter returns array with one empty string
  if sLen = 0 then
  begin
    objStr := CreateString('', VM.MemTracker);
    ArrayAppendStr(objStr);
    Dec(VM.Stack.StackTop);
    Result := CreateObject(pObj(arr));
    Exit;
  end;

  start := 1;
  while start <= sLen do
  begin
    pos := PosEx(String(delim), String(s), start);
    if pos = 0 then
    begin
      part := Copy(s, start, sLen - start + 1);
      objStr := CreateString(part, VM.MemTracker);
      ArrayAppendStr(objStr);
      Break;
    end
    else
    begin
      part := Copy(s, start, pos - start);
      objStr := CreateString(part, VM.MemTracker);
      ArrayAppendStr(objStr);
      start := pos + delimLen;
    end;
  end;

  // Handle trailing delimiter (e.g. "a," -> ["a", ""])
  if (sLen >= delimLen) and (Copy(s, sLen - delimLen + 1, delimLen) = delim) then
  begin
    objStr := CreateString('', VM.MemTracker);
    ArrayAppendStr(objStr);
  end;

  Dec(VM.Stack.StackTop); // remove GC protection
  Result := CreateObject(pObj(arr));
end;

// chr(n) -> single-character string from ASCII code
function chrNative(argCount: integer; args: pValue): TValue;
var
  code: Integer;
  ch: AnsiChar;
begin
  if argCount <> 1 then
  begin
    RuntimeError('chr() takes exactly 1 argument (ASCII code).');
    Result := CreateNilValue;
    Exit;
  end;
  if not isNumber(args[0]) then
  begin
    RuntimeError('chr() argument must be a number.');
    Result := CreateNilValue;
    Exit;
  end;
  code := AsInteger(args[0]);
  if (code < 0) or (code > 127) then
  begin
    RuntimeError('chr() argument must be 0-127.');
    Result := CreateNilValue;
    Exit;
  end;
  ch := AnsiChar(code);
  Result := CreateStringValue(AnsiString(ch));
end;

function includeTrailingBackslashNative(argCount: integer; args: pValue): TValue;
var
  s: AnsiString;
begin
  if not isString(args[0]) then
  begin
    RuntimeError('includeTrailingBackslash() argument must be a string.');
    Result := CreateNilValue;
    Exit;
  end;
  s := AsAnsiString(args[0]);
  if (Length(s) = 0) or (s[Length(s)] <> '\') then
    s := s + '\';
  Result := CreateStringValue(s);
end;

procedure RegisterStringNatives;
begin
  defineNative('strlen', strlenNative, 1);
  defineNative('substr', substrNative, 3);
  defineNative('indexOf', indexOfNative, 2);
  defineNative('charAt', charAtNative, 2);
  defineNative('upper', upperNative, 1);
  defineNative('lower', lowerNative, 1);
  defineNative('trim', trimNative, 1);
  defineNative('split', splitNative, 2);
  defineNative('chr', chrNative, 1);
  defineNative('includeTrailingBackslash', includeTrailingBackslashNative, 1);
end;

initialization
  RegisterNativeModule(RegisterStringNatives);

end.
