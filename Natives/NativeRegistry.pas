unit NativeRegistry;

interface

type
  TNativeRegistrationProc = procedure;
  TNativeShutdownProc     = procedure;

procedure RegisterNativeModule(Proc: TNativeRegistrationProc);
procedure RegisterAllNatives;

// Per-script teardown. Any native module that has UI state or
// external resources that outlive the VM (e.g. the progress dialog,
// owned by Application) can register a proc here; Suto.FreeVM invokes
// them all before dismantling the VM so state doesn't leak across
// script runs.
procedure RegisterNativeShutdown(Proc: TNativeShutdownProc);
procedure ShutdownAllNatives;

implementation

var
  Registry: array of TNativeRegistrationProc;
  Shutdowns: array of TNativeShutdownProc;

procedure RegisterNativeModule(Proc: TNativeRegistrationProc);
begin
  SetLength(Registry, Length(Registry) + 1);
  Registry[High(Registry)] := Proc;
end;

procedure RegisterAllNatives;
var
  i: Integer;
begin
  for i := 0 to High(Registry) do
    Registry[i]();
end;

procedure RegisterNativeShutdown(Proc: TNativeShutdownProc);
begin
  SetLength(Shutdowns, Length(Shutdowns) + 1);
  Shutdowns[High(Shutdowns)] := Proc;
end;

procedure ShutdownAllNatives;
var
  i: Integer;
begin
  for i := 0 to High(Shutdowns) do
    try
      Shutdowns[i]();
    except
      // Swallow shutdown exceptions to keep VM teardown best-effort.
    end;
end;

end.
