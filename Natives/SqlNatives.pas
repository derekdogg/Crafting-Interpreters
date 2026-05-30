unit SqlNatives;

interface

procedure RegisterSqlNatives;

implementation

uses
  System.SysUtils, Classes,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Stan.Intf,
  FireDAC.Stan.Async, FireDAC.Phys.MSSQL, FireDAC.DApt, FireDAC.VCLUI.Wait,
  Chunk_Types, NativeRegistry;

// ==== SQL Connection native object ====

procedure SQL_Destroy(instance: Pointer);
begin
  if TFDConnection(instance).Connected then
    TFDConnection(instance).Close;
  TFDConnection(instance).Free;
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
  stackBase : pValue;
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

  // Save stack depth for unwinding on all exit paths
  stackBase := VM.Stack.StackTop;

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
    while VM.Stack.StackTop > stackBase do Dec(VM.Stack.StackTop);
    RuntimeError('sqlConnect() config missing "server" key.');
    Exit(CreateNilValue);
  end;
  if not isString(serverVal) then
  begin
    while VM.Stack.StackTop > stackBase do Dec(VM.Stack.StackTop);
    RuntimeError('sqlConnect() "server" must be a string.');
    Exit(CreateNilValue);
  end;
  server := AsAnsiString(serverVal);

  // Required: database
  if not DictGet(dict, dbKey, dbVal) then
  begin
    while VM.Stack.StackTop > stackBase do Dec(VM.Stack.StackTop);
    RuntimeError('sqlConnect() config missing "database" key.');
    Exit(CreateNilValue);
  end;
  if not isString(dbVal) then
  begin
    while VM.Stack.StackTop > stackBase do Dec(VM.Stack.StackTop);
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
  while VM.Stack.StackTop > stackBase do Dec(VM.Stack.StackTop);

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
  stackBase : pValue;
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
  stackBase := VM.Stack.StackTop;

  // Create result array and protect from GC
  arr := newArray(VM.MemTracker);
  pushStack(VM.Stack, CreateObject(pObj(arr)));

  query := TFDQuery.Create(nil);
  try
    query.Connection := conn;
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
      while VM.Stack.StackTop > stackBase do
        Dec(VM.Stack.StackTop);
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
  stackBase : pValue;
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
  stackBase := VM.Stack.StackTop;

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
        while VM.Stack.StackTop > stackBase do
          Dec(VM.Stack.StackTop);
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
      while VM.Stack.StackTop > stackBase do
        Dec(VM.Stack.StackTop);
      RuntimeError('sqlQueryParams() failed: ' + E.Message);
      Exit(CreateNilValue);
    end;
  end;
  query.Free;

  Dec(VM.Stack.StackTop);
  Result := CreateObject(pObj(arr));
end;

procedure RegisterSqlNatives;
var
  sqlMethods : array of TNativeMethod;
begin
  SetLength(sqlMethods, 0);
  registerNativeClass('SqlConnection', sqlMethods, SQL_Destroy);
  defineNative('sqlConnect', sqlConnectNative, 1);
  defineNative('sqlQuery', sqlQueryNative, 2);
  defineNative('sqlQueryParams', sqlQueryParamsNative, 3);
  defineNative('sqlClose', sqlCloseNative, 1);
end;

initialization
  RegisterNativeModule(RegisterSqlNatives);

end.
