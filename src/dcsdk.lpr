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
  Engine,
  Utils;

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
      MessageBox(0, PChar(Format('MSYS Shell wasn''t found ("%s").', [DreamcastSoftwareDevelopmentKitRunner.MSYSExecutable])), 'Error', MB_ICONERROR);
  finally
    DreamcastSoftwareDevelopmentKitRunner.Free;
  end;
end.

