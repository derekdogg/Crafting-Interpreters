unit NativeRegistry;

interface

type
  TNativeRegistrationProc = procedure;

procedure RegisterNativeModule(Proc: TNativeRegistrationProc);
procedure RegisterAllNatives;

implementation

var
  Registry: array of TNativeRegistrationProc;

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

end.
