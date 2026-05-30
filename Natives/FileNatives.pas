unit FileNatives;

interface

  procedure RegisterFileNatives;

implementation
uses
  System.SysUtils, System.AnsiStrings, StrUtils, Chunk_Types, NativeRegistry;

function FileExistsNative(argCount: integer; args: pValue): TValue;
var
  FileName : string;

begin
  if argCount <> 1 then
  begin
    RuntimeError('fileExists() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;

  FileName := AsWideString(args[0]);

  Result := CreateBoolean(FileExists(FileName));

end;

function DirectoryExistsNative(argCount: integer; args: pValue): TValue;
var
  Directory : string;

begin
  if argCount <> 1 then
  begin
    RuntimeError('DirectoryExists() takes exactly 1 argument.');
    Result := CreateNilValue;
    Exit;
  end;

  Directory := AsWideString(args[0]);

  Result := CreateBoolean(DirectoryExists(Directory));
end;


procedure RegisterFileNatives;
begin
  defineNative('fileExists', FileExistsNative, 1);
  defineNative('directoryExists', DirectoryExistsNative, 1);
end;

initialization
  RegisterNativeModule(RegisterFileNatives);

end.
