program dcsdk;

{$mode objfpc}{$H+}

uses
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Engine;

resourcestring
  MSYSShellNotFound = 'MSYS Shell was not found ("%s").';
  ErrorTitle = 'Error';

var
  DreamcastSoftwareDevelopmentKitRunner: TDreamcastSoftwareDevelopmentKitRunner;

function ParseCommandLine: string;
var
  i: Integer;
  Sep: string;

begin
  Result := '';
  Sep := '';
  for i := 1 to ParamCount do
  begin
    Result := Result + Sep + ParamStr(i);
    Sep := ' ';
  end;
end;

{$R *.res}

begin
  DreamcastSoftwareDevelopmentKitRunner := TDreamcastSoftwareDevelopmentKitRunner.Create;
  try
    if FileExists(DreamcastSoftwareDevelopmentKitRunner.MSYSExecutable) then
    begin
      DreamcastSoftwareDevelopmentKitRunner.CommandLine := ParseCommandLine;
      DreamcastSoftwareDevelopmentKitRunner.Execute();
    end
    else
      MessageBox(0, PChar(Format(MSYSShellNotFound, [DreamcastSoftwareDevelopmentKitRunner.MSYSExecutable])),
        PChar(ErrorTitle), MB_ICONERROR);
  finally
    DreamcastSoftwareDevelopmentKitRunner.Free;
  end;
end.

