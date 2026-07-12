unit SqlNatives;

interface

procedure RegisterSqlNatives;

implementation

uses
  System.SysUtils, System.Diagnostics, Classes,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Def,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Async,
  FireDAC.Phys.MSSQL, FireDAC.DApt, FireDAC.VCLUI.Wait,
  Suto, NativeRegistry;
// NOTE: FireDAC.VCLUI.Async is deliberately NOT used. ProgressNatives
// registers our own IFDGUIxAsyncExecuteDialog implementation (backed by
// the Lox progress form) under the 'Forms' provider; linking the stock
// unit would register a competing factory for the same provider.

// ==== VM stack helpers ====
//
// The VM value stack can be REALLOCATED by any pushStack (pushStackGrow uses
// ReallocMem), so raw pValue snapshots of StackTop dangle across pushes. Save
// and unwind by DEPTH instead — same rule InvokeCallback's invariant checks
// follow. Corollary for native authors: never read `args` after the first
// pushStack either; it points into the same reallocatable buffer.

function StackDepth: NativeInt; inline;
begin
  Result := NativeInt(VM.Stack.StackTop) - NativeInt(VM.Stack.Values);
end;

procedure UnwindStackToDepth(baseDepth: NativeInt);
begin
  while NativeInt(VM.Stack.StackTop) - NativeInt(VM.Stack.Values) > baseDepth do
    Dec(VM.Stack.StackTop);
end;

// ==== SQL Connection native object ====

procedure SQL_Destroy(instance: Pointer);
begin
  if instance = nil then Exit;
  // GC sweep contract: native destructors must not raise (they run mid-sweep
  // and an exception unwinds through the collector). Close can throw on a
  // dead network or a failed implicit rollback; Free re-enters Close via
  // Destroy, so both are shielded.
  try
    if TFDConnection(instance).Connected then
      TFDConnection(instance).Close;
  except
  end;
  try
    TFDConnection(instance).Free;
  except
  end;
end;

function sqlConnectNative(argCount: integer; args: pValue): TValue;
var
  dict : pObjDictionary;
  classInfo : pNativeClassInfo;
  obj : pObjNativeObject;
  conn : TFDConnection;
  serverVal, dbVal, authVal, userVal, passVal : TValue;
  server, db, auth, user, pass : AnsiString;
  serverKey, dbKey, authKey, userKey, passKey : TValue;
  baseDepth : NativeInt;
begin
  if argCount <> 1 then
  begin
    RuntimeError('sqlConnect() takes 1 argument (config dictionary).');
    Exit(CreateNilValue);
  end;
  if not isDictionary(args[0]) then
  begin
    RuntimeError('sqlConnect() argument must be a dictionary.');
    Exit(CreateNilValue);
  end;

  dict := pObjDictionary(GetObject(args[0]));

  // Save stack depth for unwinding on all exit paths (depth, not a raw
  // pointer — pushStack may realloc the stack buffer)
  baseDepth := StackDepth;

  // Build key values for lookup ? protect each from GC before next allocation
  serverKey := CreateStringValue('server');
  pushStack(VM.Stack, serverKey );
  dbKey := CreateStringValue('database');
  pushStack(VM.Stack, dbKey );
  authKey := CreateStringValue('auth');
  pushStack(VM.Stack, authKey );
  userKey := CreateStringValue('user');
  pushStack(VM.Stack, userKey);
  passKey := CreateStringValue('password');
  pushStack(VM.Stack, passKey);

  // Required: server
  if not DictGet(dict, serverKey, serverVal) then
  begin
    UnwindStackToDepth(baseDepth);
    RuntimeError('sqlConnect() config missing "server" key.');
    Exit(CreateNilValue);
  end;
  if not isString(serverVal) then
  begin
    UnwindStackToDepth(baseDepth);
    RuntimeError('sqlConnect() "server" must be a string.');
    Exit(CreateNilValue);
  end;
  server := AsAnsiString(serverVal);

  // Required: database
  if not DictGet(dict, dbKey, dbVal) then
  begin
    UnwindStackToDepth(baseDepth);
    RuntimeError('sqlConnect() config missing "database" key.');
    Exit(CreateNilValue);
  end;
  if not isString(dbVal) then
  begin
    UnwindStackToDepth(baseDepth);
    RuntimeError('sqlConnect() "database" must be a string.');
    Exit(CreateNilValue);
  end;
  db := AsAnsiString(dbVal);

  // Optional: auth (default "windows")
  auth := 'windows';
  if DictGet(dict, authKey, authVal) then
  begin
    if isString(authVal) then
      auth := AnsiString(LowerCase(String(AsAnsiString(authVal))));
  end;

  // Optional: user, password (for SQL auth)
  user := '';
  pass := '';
  if DictGet(dict, userKey, userVal) then
    if isString(userVal) then
      user := AsAnsiString(userVal);
  if DictGet(dict, passKey, passVal) then
    if isString(passVal) then
      pass := AsAnsiString(passVal);

  // Keys no longer needed ? unwind stack
  UnwindStackToDepth(baseDepth);

  // Create and configure the connection
  conn := TFDConnection.Create(nil);
  try
    conn.DriverName := 'MSSQL';
    conn.Params.Values['Server'] := string(server);
    conn.Params.Database := string(db);

    if auth = 'sql' then
    begin
      conn.Params.UserName := string(user);
      conn.Params.Password := string(pass);
    end
    else
    begin
      conn.Params.Values['OSAuthent'] := 'Yes';
    end;

    conn.LoginPrompt := False;
    conn.Open;
  except
    on E: Exception do
    begin
      conn.Free;
      RuntimeError('sqlConnect() failed: ' + E.Message);
      Exit(CreateNilValue);
    end;
  end;

  classInfo := findNativeClass('SqlConnection');
  Assert(classInfo <> nil, 'SqlConnection class not registered');
  obj := newNativeObject(conn, classInfo, VM.MemTracker);
  Result := CreateObject(pObj(obj));
end;

function sqlCloseNative(argCount: integer; args: pValue): TValue;
var
  nativeObj : pObjNativeObject;
  conn : TFDConnection;
begin
  if argCount <> 1 then
  begin
    RuntimeError('sqlClose() takes 1 argument (connection).');
    Exit(CreateNilValue);
  end;
  if not isNativeObject(args[0]) then
  begin
    RuntimeError('sqlClose() argument must be a SQL connection.');
    Exit(CreateNilValue);
  end;
  nativeObj := pObjNativeObject(GetObject(args[0]));
  if nativeObj^.classInfo^.name <> 'SqlConnection' then
  begin
    RuntimeError('sqlClose() argument must be a SQL connection.');
    Exit(CreateNilValue);
  end;
  if nativeObj^.instance = nil then
  begin
    RuntimeError('sqlClose() connection has been released by the host.');
    Exit(CreateNilValue);
  end;
  conn := TFDConnection(nativeObj^.instance);
  if conn.Connected then
    conn.Close;
  Result := CreateNilValue;
end;

function sqlQueryNative(argCount: integer; args: pValue): TValue;
var
  nativeObj : pObjNativeObject;
  conn : TFDConnection;
  query : TFDQuery;
  sql : AnsiString;
  arr : pObjArray;
  dict : pObjDictionary;
  colCount, i : integer;
  colName : AnsiString;
  keyVal, valVal : TValue;
  baseDepth : NativeInt;
begin
  if argCount <> 2 then
  begin
    RuntimeError('sqlQuery() takes 2 arguments (connection, sql string).');
    Exit(CreateNilValue);
  end;
  if not isNativeObject(args[0]) then
  begin
    RuntimeError('sqlQuery() first argument must be a SQL connection.');
    Exit(CreateNilValue);
  end;
  nativeObj := pObjNativeObject(GetObject(args[0]));
  if nativeObj^.classInfo^.name <> 'SqlConnection' then
  begin
    RuntimeError('sqlQuery() first argument must be a SQL connection.');
    Exit(CreateNilValue);
  end;
  if nativeObj^.instance = nil then
  begin
    RuntimeError('sqlQuery() connection has been released by the host.');
    Exit(CreateNilValue);
  end;
  if not isString(args[1]) then
  begin
    RuntimeError('sqlQuery() second argument must be a string.');
    Exit(CreateNilValue);
  end;

  conn := TFDConnection(nativeObj^.instance);
  if not conn.Connected then
  begin
    RuntimeError('sqlQuery() connection is not open.');
    Exit(CreateNilValue);
  end;

  sql := AsAnsiString(args[1]);

  // Save stack depth so we can unwind on any exit path
  baseDepth := StackDepth;

  // Create result array and protect from GC
  arr := newArray(VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(arr)));

  query := TFDQuery.Create(nil);
  try
    query.Connection := conn;
    // Raw SQL: no FireDAC parameter/macro scanning, so bare ':' or '&'
    // outside string literals can't be misread as :params / &macros.
    // (sqlQueryParams keeps scanning on — it needs :p0, :p1, ...)
    query.ResourceOptions.ParamCreate := False;
    query.ResourceOptions.MacroCreate := False;
    // Non-blocking, same UX as sqlExec: FireDAC executes on its internal
    // thread and pumps messages here, with input restricted to the Lox
    // progress dialog (Cancel aborts; surfaces as a runtime error here).
    // fmAll pulls the ENTIRE resultset inside the pumped Open, so the
    // row-copy loop below reads from memory and never blocks on the network.
    query.ResourceOptions.CmdExecMode := amCancelDialog;
    query.FetchOptions.Mode := fmAll;
    query.SQL.Text := string(sql);
    query.Open;

    colCount := query.FieldCount;

    while not query.Eof do
    begin
      // Create a dictionary for this row, protect from GC
      dict := newDictionary(VM.MemTracker);
      pushStack(VM.Stack, CreateObject(pObj(dict)));

      for i := 0 to colCount - 1 do
      begin
        // Create key string (column name), protect from GC
        colName := AnsiString(query.Fields[i].FieldName);
        keyVal := CreateStringValue(colName);
        pushStack(VM.Stack, keyVal);

        // Create value based on field type
        if query.Fields[i].IsNull then
          valVal := CreateNilValue
        else if query.Fields[i].DataType in [ftInteger, ftSmallint, ftWord, ftLargeint,
            ftFloat, ftCurrency, ftBCD, ftFMTBcd] then
          valVal := CreateNumber(query.Fields[i].AsFloat)
        else if query.Fields[i].DataType in [ftBoolean] then
          valVal := CreateBoolean(query.Fields[i].AsBoolean)
        else
        begin
          // Everything else as string
          valVal := CreateStringValue(AnsiString(query.Fields[i].AsString));
        end;

        // Protect valVal from GC during DictSet (which may trigger table growth)
        pushStack(VM.Stack, valVal);
        DictSet(dict, keyVal, valVal, VM.MemTracker);
        Dec(VM.Stack.StackTop); // pop valVal
        Dec(VM.Stack.StackTop); // pop keyVal
      end;

      // Append dict to array (GC-safe growth)
      EnsureArrayCapacity(arr, VM.MemTracker);
      arr^.Elements[arr^.Count] := CreateObject(pObj(dict));
      Inc(arr^.Count);

      // Pop dict from GC protection
      Dec(VM.Stack.StackTop);

      query.Next;
    end;

    query.Close;
  except
    on E: Exception do
    begin
      query.Free;
      // Unwind stack to saved depth
      UnwindStackToDepth(baseDepth);
      RuntimeError('sqlQuery() failed: ' + E.Message);
      Exit(CreateNilValue);
    end;
  end;
  query.Free;

  // Pop array from GC protection
  Dec(VM.Stack.StackTop);
  Result := CreateObject(pObj(arr));
end;

function sqlQueryParamsNative(argCount: integer; args: pValue): TValue;
var
  nativeObj : pObjNativeObject;
  conn : TFDConnection;
  query : TFDQuery;
  sql : AnsiString;
  arr : pObjArray;
  params : pObjArray;
  dict : pObjDictionary;
  colCount, i, p : integer;
  colName : AnsiString;
  keyVal, valVal, paramVal : TValue;
  baseDepth : NativeInt;
begin
  if argCount <> 3 then
  begin
    RuntimeError('sqlQueryParams() takes 3 arguments (connection, sql, params array).');
    Exit(CreateNilValue);
  end;
  if not isNativeObject(args[0]) then
  begin
    RuntimeError('sqlQueryParams() first argument must be a SQL connection.');
    Exit(CreateNilValue);
  end;
  nativeObj := pObjNativeObject(GetObject(args[0]));
  if nativeObj^.classInfo^.name <> 'SqlConnection' then
  begin
    RuntimeError('sqlQueryParams() first argument must be a SQL connection.');
    Exit(CreateNilValue);
  end;
  if nativeObj^.instance = nil then
  begin
    RuntimeError('sqlQueryParams() connection has been released by the host.');
    Exit(CreateNilValue);
  end;
  if not isString(args[1]) then
  begin
    RuntimeError('sqlQueryParams() second argument must be a SQL string.');
    Exit(CreateNilValue);
  end;
  if not isArray(args[2]) then
  begin
    RuntimeError('sqlQueryParams() third argument must be a params array.');
    Exit(CreateNilValue);
  end;

  conn := TFDConnection(nativeObj^.instance);
  if not conn.Connected then
  begin
    RuntimeError('sqlQueryParams() connection is not open.');
    Exit(CreateNilValue);
  end;

  sql := AsAnsiString(args[1]);
  params := pObjArray(GetObject(args[2]));

  // Save stack depth so we can unwind on any exit path
  baseDepth := StackDepth;

  // Create result array and protect from GC
  arr := newArray(VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(arr)));

  query := TFDQuery.Create(nil);
  try
    query.Connection := conn;
    query.SQL.Text := string(sql);

    // Bind parameters: :p0, :p1, :p2, etc.
    for p := 0 to params^.Count - 1 do
    begin
      paramVal := params^.Elements[p];
      if isNumber(paramVal) then
        query.ParamByName('p' + IntToStr(p)).AsFloat := GetNumber(paramVal)
      else if isBoolean(paramVal) then
        query.ParamByName('p' + IntToStr(p)).AsBoolean := GetBoolean(paramVal)
      else if isNill(paramVal) then
        query.ParamByName('p' + IntToStr(p)).Clear
      else if isString(paramVal) then
        query.ParamByName('p' + IntToStr(p)).AsString := string(AsAnsiString(paramVal))
      else
      begin
        RuntimeError('sqlQueryParams() parameter ' + IntToStr(p) + ' must be a string, number, boolean, or nil.');
        query.Free;
        UnwindStackToDepth(baseDepth);
        Exit(CreateNilValue);
      end;
    end;

    query.Open;

    colCount := query.FieldCount;

    while not query.Eof do
    begin
      dict := newDictionary(VM.MemTracker);
      pushStack(VM.Stack, CreateObject(pObj(dict)));

      for i := 0 to colCount - 1 do
      begin
        colName := AnsiString(query.Fields[i].FieldName);
        keyVal := CreateStringValue(colName);
        pushStack(VM.Stack, keyVal);

        if query.Fields[i].IsNull then
          valVal := CreateNilValue
        else if query.Fields[i].DataType in [ftInteger, ftSmallint, ftWord, ftLargeint,
            ftFloat, ftCurrency, ftBCD, ftFMTBcd] then
          valVal := CreateNumber(query.Fields[i].AsFloat)
        else if query.Fields[i].DataType in [ftBoolean] then
          valVal := CreateBoolean(query.Fields[i].AsBoolean)
        else
          valVal := CreateStringValue(AnsiString(query.Fields[i].AsString));

        // Protect valVal from GC during DictSet (which may trigger table growth)
        pushStack(VM.Stack, valVal);
        DictSet(dict, keyVal, valVal, VM.MemTracker);
        Dec(VM.Stack.StackTop); // pop valVal
        Dec(VM.Stack.StackTop); // pop keyVal
      end;

      EnsureArrayCapacity(arr, VM.MemTracker);
      arr^.Elements[arr^.Count] := CreateObject(pObj(dict));
      Inc(arr^.Count);

      Dec(VM.Stack.StackTop); // pop dict
      query.Next;
    end;

    query.Close;
  except
    on E: Exception do
    begin
      query.Free;
      // Unwind stack to saved depth
      UnwindStackToDepth(baseDepth);
      RuntimeError('sqlQueryParams() failed: ' + E.Message);
      Exit(CreateNilValue);
    end;
  end;
  query.Free;

  Dec(VM.Stack.StackTop);
  Result := CreateObject(pObj(arr));
end;

// ==== sqlExec: non-blocking statement execution (PoC) ====
//
// Runs a statement with FireDAC CmdExecMode=amCancelDialog: FireDAC executes
// the command on an internal thread and pumps messages on this (the VM/VCL)
// thread until it finishes. Input is restricted to the Lox progress dialog
// (ProgressNatives registers it as FireDAC's async dialog service), whose
// Cancel button / Escape aborts the statement via the executor's AbortJob.
// The Lox script stays fully synchronous.
//
// Returns a dictionary:
//   [ "error": nil|string, "cancelled": bool, "rowsAffected": n, "elapsedMs": n ]
// SQL failures come back in .error (lenient) rather than as Lox runtime
// errors, so a migration-style script can log, roll back, and abort itself.
// A user cancel sets both .error and .cancelled=true.

function sqlExecNative(argCount: integer; args: pValue): TValue;
var
  nativeObj : pObjNativeObject;
  conn : TFDConnection;
  sql : AnsiString;
  dict : pObjDictionary;
  keyVal, valVal : TValue;
  rows : Integer;
  errMsg : string;
  cancelled : Boolean;
  sw : TStopwatch;
  savedMode : TFDStanAsyncMode;
  savedParamCreate, savedMacroCreate : Boolean;
begin
  if argCount <> 2 then
  begin
    RuntimeError('sqlExec() takes 2 arguments (connection, sql string).');
    Exit(CreateNilValue);
  end;
  if not isNativeObject(args[0]) then
  begin
    RuntimeError('sqlExec() first argument must be a SQL connection.');
    Exit(CreateNilValue);
  end;
  nativeObj := pObjNativeObject(GetObject(args[0]));
  if nativeObj^.classInfo^.name <> 'SqlConnection' then
  begin
    RuntimeError('sqlExec() first argument must be a SQL connection.');
    Exit(CreateNilValue);
  end;
  if nativeObj^.instance = nil then
  begin
    RuntimeError('sqlExec() connection has been released by the host.');
    Exit(CreateNilValue);
  end;
  if not isString(args[1]) then
  begin
    RuntimeError('sqlExec() second argument must be a string.');
    Exit(CreateNilValue);
  end;

  conn := TFDConnection(nativeObj^.instance);
  if not conn.Connected then
  begin
    RuntimeError('sqlExec() connection is not open.');
    Exit(CreateNilValue);
  end;

  sql := AsAnsiString(args[1]);

  rows := 0;
  errMsg := '';
  cancelled := False;
  savedMode := conn.ResourceOptions.CmdExecMode;
  savedParamCreate := conn.ResourceOptions.ParamCreate;
  savedMacroCreate := conn.ResourceOptions.MacroCreate;
  conn.ResourceOptions.CmdExecMode := amCancelDialog;
  // Raw SQL: migration DDL can legitimately contain ':' or '&' outside
  // string literals; without this they'd be misread as :params / &macros.
  conn.ResourceOptions.ParamCreate := False;
  conn.ResourceOptions.MacroCreate := False;
  sw := TStopwatch.StartNew;
  try
    try
      rows := conn.ExecSQL(string(sql));
    except
      // A user abort surfaces as EAbort: the executor maps an ekCmdAborted
      // engine exception in the aborted state to Abort (FireDAC.Stan.Async).
      on E: EAbort do
      begin
        cancelled := True;
        errMsg := 'Cancelled by user.';
      end;
      on E: Exception do
        errMsg := E.Message;
    end;
  finally
    sw.Stop;
    conn.ResourceOptions.CmdExecMode := savedMode;
    conn.ResourceOptions.ParamCreate := savedParamCreate;
    conn.ResourceOptions.MacroCreate := savedMacroCreate;
  end;

  // Build the result dict, protecting every intermediate from GC
  dict := newDictionary(VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(dict)));

  keyVal := CreateStringValue('error');
  pushStack(VM.Stack, keyVal);
  if errMsg = '' then
    valVal := CreateNilValue
  else
    valVal := CreateStringValue(AnsiString(errMsg));
  pushStack(VM.Stack, valVal);
  DictSet(dict, keyVal, valVal, VM.MemTracker);
  Dec(VM.Stack.StackTop); // pop valVal
  Dec(VM.Stack.StackTop); // pop keyVal

  keyVal := CreateStringValue('cancelled');
  pushStack(VM.Stack, keyVal);
  valVal := CreateBoolean(cancelled);
  pushStack(VM.Stack, valVal);
  DictSet(dict, keyVal, valVal, VM.MemTracker);
  Dec(VM.Stack.StackTop);
  Dec(VM.Stack.StackTop);

  keyVal := CreateStringValue('rowsAffected');
  pushStack(VM.Stack, keyVal);
  valVal := CreateNumber(rows);
  pushStack(VM.Stack, valVal);
  DictSet(dict, keyVal, valVal, VM.MemTracker);
  Dec(VM.Stack.StackTop);
  Dec(VM.Stack.StackTop);

  keyVal := CreateStringValue('elapsedMs');
  pushStack(VM.Stack, keyVal);
  valVal := CreateNumber(sw.ElapsedMilliseconds);
  pushStack(VM.Stack, valVal);
  DictSet(dict, keyVal, valVal, VM.MemTracker);
  Dec(VM.Stack.StackTop);
  Dec(VM.Stack.StackTop);

  Dec(VM.Stack.StackTop); // pop dict
  Result := CreateObject(pObj(dict));
end;

procedure RegisterSqlNatives;
var
  sqlMethods : array of TNativeMethod;
begin
  SetLength(sqlMethods, 0);
  registerNativeClass('SqlConnection', sqlMethods, SQL_Destroy);
  defineNative('sqlConnect', sqlConnectNative, 1);
  defineNative('sqlExec', sqlExecNative, 2);
  defineNative('sqlQuery', sqlQueryNative, 2);
  defineNative('sqlQueryParams', sqlQueryParamsNative, 3);
  defineNative('sqlClose', sqlCloseNative, 1);
end;

initialization
  RegisterNativeModule(RegisterSqlNatives);

end.
