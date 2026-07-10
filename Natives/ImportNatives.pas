unit ImportNatives;

// ============================================================
// Module import natives (docs/require-import-proposal.md)
//
//   require(path) -> dict
//     Loads, compiles, executes, and caches the named .lox file (or a
//     registered native module) and returns its exported names as a dict.
//     - Everything defined at a module's top level (var / fun / record)
//       is exported.
//     - Re-requiring the same canonical path returns the same dict object.
//     - Cycles are legal: a re-entrant require of a module that is still
//       loading returns its (partially populated) dict immediately.
//     - Paths resolve relative to the importing file's directory (or the
//       host-set script dir / CWD for the top-level script). '.lox' is
//       appended only when the request has no extension at all.
//     - A module whose load fails is marked Failed and is re-attempted on
//       the next require (failures don't poison the cache).
//
//   Native modules are registered from Pascal via RegisterNativeLoxModule
//   (two-tier registry: builders here, instances in the module cache).
//   A tiny 'demo' module (version, twice(x)) exercises this path.
// ============================================================

interface

uses
  Suto;

type
  // Populates a freshly created (and already GC-rooted) module dict.
  TNativeModuleBuilder = procedure(dict : pObjDictionary);

procedure RegisterImportNatives;

// Registers a Pascal-implemented module under `name`. The builder runs once,
// on the first require(name); later requires return the cached dict.
procedure RegisterNativeLoxModule(const name : string; builder : TNativeModuleBuilder);

// Base directory used to resolve requires made by the top-level script
// (which reaches the VM as bare source text, without a path). Hosts should
// call this with the script file's directory before running it; when unset,
// resolution falls back to the current working directory.
procedure SetScriptBaseDir(const dir : string);

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Generics.Collections,
  NativeRegistry;

type
  TModuleState = (msLoading, msLoaded, msFailed);

  PModuleEntry = ^TModuleEntry;
  TModuleEntry = record
    // Heap-allocated so the TValue slot has a stable address for
    // RegisterGCRoot — the root keeps the module dict alive from the moment
    // the entry is created (before its body runs) for the VM's lifetime.
    DictValue : TValue;
    State     : TModuleState;
  end;

var
  // Canonical path (lowercased) or native-module name -> cache entry.
  ModuleCache    : TDictionary<string, PModuleEntry>;
  // Native-module name -> builder. Populated at unit init / by hosts;
  // survives across VM restarts (like defineNative registrations).
  NativeBuilders : TDictionary<string, TNativeModuleBuilder>;
  // Full paths of modules currently executing their body, innermost last.
  // Top of stack supplies the base dir for relative resolves.
  ImportStack    : TList<string>;
  ScriptBaseDir  : string;

procedure SetScriptBaseDir(const dir : string);
begin
  ScriptBaseDir := dir;
end;

procedure RegisterNativeLoxModule(const name : string; builder : TNativeModuleBuilder);
begin
  NativeBuilders.AddOrSetValue(name, builder);
end;

// Entries hold a GC root pointing at their heap-allocated DictValue slot.
// Unregister the root before Dispose so that if a VM is still live when this
// runs (unit-finalization order at shutdown, in particular), its ExtraRoots
// list never retains a pointer into freed PModuleEntry memory.
// UnregisterGCRoot is a no-op when the slot isn't in the list, so the
// InitVM path (where the previous VM's roots are already gone) stays safe.
procedure ClearModuleCache;
var
  entry : PModuleEntry;
begin
  for entry in ModuleCache.Values do
  begin
    UnregisterGCRoot(entry^.DictValue);
    Dispose(entry);
  end;
  ModuleCache.Clear;
  ImportStack.Clear;
end;

// Path resolution per proposal §4.1: relative to the importing file (falling
// back to the host-set script dir, then CWD), '.lox' appended only when no
// extension is present, then canonicalised with ExpandFileName.
function ResolveModulePath(const raw : string; out fullPath : string) : Boolean;
var
  candidate, base : string;
begin
  candidate := StringReplace(raw, '/', '\', [rfReplaceAll]);
  if ExtractFileExt(candidate) = '' then
    candidate := candidate + '.lox';

  if TPath.IsPathRooted(candidate) then
    fullPath := ExpandFileName(candidate)
  else
  begin
    if ImportStack.Count > 0 then
      base := ExtractFilePath(ImportStack[ImportStack.Count - 1])
    else if ScriptBaseDir <> '' then
      base := ScriptBaseDir
    else
      base := GetCurrentDir;
    fullPath := ExpandFileName(TPath.Combine(base, candidate));
  end;
  Result := FileExists(fullPath);
end;

function NewCacheEntry(const key : string; dict : pObjDictionary;
  state : TModuleState) : PModuleEntry;
begin
  New(Result);
  Result^.DictValue := CreateObject(pObj(dict));
  Result^.State := state;
  RegisterGCRoot(Result^.DictValue);
  ModuleCache.Add(key, Result);
end;

function RequireNative(argCount : integer; args : pValue) : TValue;
var
  raw, fullPath, key : string;
  content : AnsiString;
  entry : PModuleEntry;
  dict : pObjDictionary;
  builder : TNativeModuleBuilder;
  closure : pObjClosure;
  closureVal, ret : TValue;
  res : TInterpretResult;
begin
  Result := CreateNilValue;
  if not isString(args[0]) then
  begin
    RuntimeError('require: expected string path');
    Exit;
  end;
  raw := AsWideString(args[0]);

  // --- Tier 1: registered native module (bare name, no path separator).
  if (Pos('/', raw) = 0) and (Pos('\', raw) = 0) and
     NativeBuilders.TryGetValue(raw, builder) then
  begin
    if ModuleCache.TryGetValue(raw, entry) then
      Exit(entry^.DictValue);
    dict := newDictionary(VM.MemTracker);
    entry := NewCacheEntry(raw, dict, msLoaded);
    builder(dict);
    Exit(entry^.DictValue);
  end;

  // --- Tier 2: .lox file.
  if not ResolveModulePath(raw, fullPath) then
  begin
    RuntimeError('require: module ''' + raw + ''' not found');
    Exit;
  end;
  // Windows filesystem is case-insensitive; fold the canonical path so
  // require("Lib/X.lox") and require("lib/x.lox") share one cache entry.
  key := AnsiLowerCase(fullPath);

  if ModuleCache.TryGetValue(key, entry) then
  begin
    if entry^.State in [msLoaded, msLoading] then
      // msLoading is the cycle case: hand back the in-flight (partial) dict.
      Exit(entry^.DictValue);
    // msFailed: retire the entry and re-attempt the load from scratch.
    UnregisterGCRoot(entry^.DictValue);
    Dispose(entry);
    ModuleCache.Remove(key);
  end;

  try
    content := AnsiString(TFile.ReadAllText(fullPath));
  except
    on E: Exception do
    begin
      RuntimeError('require: cannot open ''' + fullPath + ''': ' + E.Message);
      Exit;
    end;
  end;

  dict := newDictionary(VM.MemTracker);
  entry := NewCacheEntry(key, dict, msLoading);

  closure := CompileModuleSource(PAnsiChar(content), dict);
  if closure = nil then
  begin
    entry^.State := msFailed;
    RuntimeError('require: compile failed in ''' + fullPath + ''': ' + Parser.ErrorStr);
    Exit;
  end;

  // Root the closure across InvokeCallback's entry (its first pushStack may
  // grow the stack and collect an unrooted closure).
  closureVal := CreateObject(pObj(closure));
  RegisterGCRoot(closureVal);
  ImportStack.Add(fullPath);
  try
    res := InvokeCallback(closureVal, [], ret);
  finally
    ImportStack.Delete(ImportStack.Count - 1);
    UnregisterGCRoot(closureVal);
  end;

  if res.code <> INTERPRET_OK then
  begin
    // Runtime error inside the module body: the message is already set and
    // propagates out of require() as a normal runtime error.
    entry^.State := msFailed;
    Exit;
  end;

  entry^.State := msLoaded;
  Result := entry^.DictValue;
end;

// ---- Demo native module: require("demo") -----------------------------------

function DemoTwiceNative(argCount : integer; args : pValue) : TValue;
begin
  if not isNumber(args[0]) then
  begin
    RuntimeError('twice() expects a number.');
    Exit(CreateNilValue);
  end;
  Result := CreateNumber(GetNumber(args[0]) * 2);
end;

procedure PopulateDemoModule(dict : pObjDictionary);
var
  k, v : TValue;
begin
  // The dict itself is rooted by its cache entry; key/value need their own
  // roots across the allocating DictSet calls.
  k := CreateNilValue;
  v := CreateNilValue;
  RegisterGCRoot(k);
  RegisterGCRoot(v);
  try
    k := CreateStringValue('version');
    v := CreateStringValue('1.0');
    DictSet(dict, k, v, VM.MemTracker);

    k := CreateStringValue('twice');
    v := CreateObject(pObj(newNative(DemoTwiceNative, 1, 'twice', VM.MemTracker)));
    DictSet(dict, k, v, VM.MemTracker);
  finally
    UnregisterGCRoot(v);
    UnregisterGCRoot(k);
  end;
end;

// -----------------------------------------------------------------------------

procedure RegisterImportNatives;
begin
  // Runs from RegisterAllNatives inside every InitVM: the previous VM (and
  // with it every registered root) is gone, so retire stale cache entries.
  ClearModuleCache;
  defineNative('require', RequireNative, 1);
  RegisterNativeLoxModule('demo', PopulateDemoModule);
end;

initialization
  ModuleCache := TDictionary<string, PModuleEntry>.Create;
  NativeBuilders := TDictionary<string, TNativeModuleBuilder>.Create;
  ImportStack := TList<string>.Create;
  RegisterNativeModule(RegisterImportNatives);

finalization
  ClearModuleCache;
  ModuleCache.Free;
  NativeBuilders.Free;
  ImportStack.Free;

end.
