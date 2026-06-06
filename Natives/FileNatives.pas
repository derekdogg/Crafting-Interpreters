unit FileNatives;

// ============================================================
// File I/O Natives
//
//   fileExists(path) -> bool
//     Returns true if the file exists on disk.
//
//   directoryExists(path) -> bool
//     Returns true if the directory exists on disk.
//
//   readFile(path) -> string
//     Reads entire file contents as a string. Runtime error if not found.
//
//   writeFile(path, content) -> nil
//     Writes a string to a file (overwrites if exists, creates if not).
//
//   writeCsv(path, rows) -> nil
//     Writes an array of dictionaries to a CSV file.
//     Column headers are taken from the keys of the first row.
//     Values are stringified. Strings containing commas, quotes, or
//     newlines are quoted per RFC 4180.
// ============================================================

interface

  procedure RegisterFileNatives;

implementation
uses
  System.SysUtils, System.Classes, System.IOUtils, System.AnsiStrings, StrUtils,
  Chunk_Types, NativeRegistry;

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

function readFileNative(argCount: integer; args: pValue): TValue;
var
  FileName, Content: string;
begin
  if argCount <> 1 then
  begin
    RuntimeError('readFile() takes exactly 1 argument.');
    Exit(CreateNilValue);
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('readFile() argument must be a string.');
    Exit(CreateNilValue);
  end;
  FileName := AsWideString(args[0]);
  if not FileExists(FileName) then
  begin
    RuntimeError('readFile(): file not found: ' + FileName);
    Exit(CreateNilValue);
  end;
  Content := TFile.ReadAllText(FileName);
  Result := CreateStringValue(AnsiString(Content));
end;

function writeFileNative(argCount: integer; args: pValue): TValue;
var
  FileName, Content: string;
begin
  if argCount <> 2 then
  begin
    RuntimeError('writeFile() takes exactly 2 arguments (path, content).');
    Exit(CreateNilValue);
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('writeFile() first argument must be a string (path).');
    Exit(CreateNilValue);
  end;
  if not isString(args[1]) then
  begin
    RuntimeError('writeFile() second argument must be a string (content).');
    Exit(CreateNilValue);
  end;
  FileName := AsWideString(args[0]);
  Content := AsWideString(args[1]);
  TFile.WriteAllText(FileName, Content);
  Result := CreateNilValue;
end;

// CSV-escape a single cell value per RFC 4180:
// If the value contains a comma, double-quote, or newline, wrap it in
// double-quotes and double any internal quotes.
function CsvEscape(const S: string): string;
begin
  if (Pos(',', S) > 0) or (Pos('"', S) > 0) or
     (Pos(#10, S) > 0) or (Pos(#13, S) > 0) then
    Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := S;
end;

function writeCsvNative(argCount: integer; args: pValue): TValue;
var
  FileName: string;
  rows: pObjArray;
  dict: pObjDictionary;
  colCount, i, j: Integer;
  sb: TStringBuilder;
  entry: pDictEntry;
  colKeys: array of TValue;
  colNames: array of string;
  val: TValue;
  cellStr: string;
begin
  if argCount <> 2 then
  begin
    RuntimeError('writeCsv() takes exactly 2 arguments (path, rows).');
    Exit(CreateNilValue);
  end;
  if not isString(args[0]) then
  begin
    RuntimeError('writeCsv() first argument must be a string (path).');
    Exit(CreateNilValue);
  end;
  if not isArray(args[1]) then
  begin
    RuntimeError('writeCsv() second argument must be an array of dictionaries.');
    Exit(CreateNilValue);
  end;
  FileName := AsWideString(args[0]);
  rows := pObjArray(GetObject(args[1]));
  if rows^.Count = 0 then
  begin
    TFile.WriteAllText(FileName, '');
    Exit(CreateNilValue);
  end;

  // First row determines column headers
  if not isDictionary(rows^.Elements[0]) then
  begin
    RuntimeError('writeCsv() rows must contain dictionaries.');
    Exit(CreateNilValue);
  end;
  dict := pObjDictionary(GetObject(rows^.Elements[0]));

  // Extract column keys from the first dictionary
  colCount := 0;
  SetLength(colKeys, dict^.Capacity);
  SetLength(colNames, dict^.Capacity);
  for i := 0 to dict^.Capacity - 1 do
  begin
    entry := @dict^.Entries[i];
    if entry^.occupied and (not entry^.tombstone) then
    begin
      colKeys[colCount] := entry^.key;
      colNames[colCount] := ValueToStr(entry^.key);
      Inc(colCount);
    end;
  end;
  SetLength(colKeys, colCount);
  SetLength(colNames, colCount);

  sb := TStringBuilder.Create;
  try
    // Write header row
    for j := 0 to colCount - 1 do
    begin
      if j > 0 then sb.Append(',');
      sb.Append(CsvEscape(colNames[j]));
    end;
    sb.AppendLine;

    // Write data rows
    for i := 0 to rows^.Count - 1 do
    begin
      if not isDictionary(rows^.Elements[i]) then Continue;
      dict := pObjDictionary(GetObject(rows^.Elements[i]));
      for j := 0 to colCount - 1 do
      begin
        if j > 0 then sb.Append(',');
        if DictGet(dict, colKeys[j], val) then
          cellStr := ValueToStr(val)
        else
          cellStr := '';
        sb.Append(CsvEscape(cellStr));
      end;
      sb.AppendLine;
    end;

    TFile.WriteAllText(FileName, sb.ToString);
  finally
    sb.Free;
  end;
  Result := CreateNilValue;
end;


procedure RegisterFileNatives;
begin
  defineNative('fileExists', FileExistsNative, 1);
  defineNative('directoryExists', DirectoryExistsNative, 1);
  defineNative('readFile', readFileNative, 1);
  defineNative('writeFile', writeFileNative, 2);
  defineNative('writeCsv', writeCsvNative, 2);
end;

initialization
  RegisterNativeModule(RegisterFileNatives);

end.
