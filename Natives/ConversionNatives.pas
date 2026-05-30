unit ConversionNatives;

interface

procedure RegisterConversionNatives;

implementation

uses
  System.SysUtils, Chunk_Types, NativeRegistry;

// ---- Conversion native functions ----

function strNative(argCount: integer; args: pValue): TValue;
var
  s : AnsiString;
begin
  if argCount <> 1 then
  begin
    RuntimeError('str() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  s := AnsiString(ValueToStr(args[0]));
  Result := CreateStringValue(s);
end;

function numNative(argCount: integer; args: pValue): TValue;
var
  s : AnsiString;
  d : Double;
  fs : TFormatSettings;
begin
  if argCount <> 1 then
  begin
    RuntimeError('num() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if isNumber(args[0]) then
    Result := args[0]
  else if isBoolean(args[0]) then
  begin
    if GetBoolean(args[0]) then
      Result := CreateNumber(1)
    else
      Result := CreateNumber(0);
  end
  else if isObject(args[0]) and isString(args[0]) then
  begin
    s := AsAnsiString(args[0]);
    fs := TFormatSettings.Create;
    fs.DecimalSeparator := '.';
    if TryStrToFloat(String(s), d, fs) then
      Result := CreateNumber(d)
    else
      Result := CreateNilValue;
  end
  else
    Result := CreateNilValue;
end;

function boolNative(argCount: integer; args: pValue): TValue;
begin
  if argCount <> 1 then
  begin
    RuntimeError('bool() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  Result := CreateBoolean(not IsFalsey(args[0]));
end;

function typeNative(argCount: integer; args: pValue): TValue;
var
  s : AnsiString;
begin
  if argCount <> 1 then
  begin
    RuntimeError('type() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;
  if isNumber(args[0]) then
    s := 'number'
  else if isBoolean(args[0]) then
    s := 'boolean'
  else if isNill(args[0]) then
    s := 'nil'
  else if isObject(args[0]) then
  begin
    case GetObject(args[0]).ObjectKind of
      okString:       s := 'string';
      okFunction:     s := 'function';
      okNative:       s := 'function';
      okClosure:      s := 'function';
      okArray:        s := 'array';
      okRecordType:   s := 'record_type';
      okRecord:       s := 'record';
      okNativeObject: s := 'native_object';
      okDictionary:   s := 'dictionary';
    else
      s := 'unknown';
    end;
  end
  else
    s := 'unknown';
  Result := CreateStringValue(s);
end;

procedure RegisterConversionNatives;
begin
  defineNative('str', strNative, 1);
  defineNative('num', numNative, 1);
  defineNative('bool', boolNative, 1);
  defineNative('type', typeNative, 1);
end;

initialization
  RegisterNativeModule(RegisterConversionNatives);

end.
