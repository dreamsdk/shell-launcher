program DCSDK;

{$mode objfpc}{$H+}
{$R *.res}

uses
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Classes,
  Engine;

resourcestring
  MSYSShellNotFound = 'MinGW/MSYS is not properly installed.';
  ErrorTitle = 'Error';

var
  DreamcastSoftwareDevelopmentKitRunner: TDreamcastSoftwareDevelopmentKitRunner;
  IsAutomatedCall: Boolean;

function ParseCommandLine: string;
var
  i: Integer;
  Sep, Param: string;

begin
  Result := '';
  Sep := '';
  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);
    if LowerCase(Param) <> '--dcsdk-automated-call' then
    begin
      Result := Result + Sep + Param;
      Sep := ' ';
    end
    else
      IsAutomatedCall := True;
  end;
end;

begin
  IsAutomatedCall := False;
  DreamcastSoftwareDevelopmentKitRunner := TDreamcastSoftwareDevelopmentKitRunner.Create;
  try
    if DreamcastSoftwareDevelopmentKitRunner.Healthy then
    begin
      DreamcastSoftwareDevelopmentKitRunner.CommandLine := ParseCommandLine;
      DreamcastSoftwareDevelopmentKitRunner.AutomatedCall := IsAutomatedCall;
      DreamcastSoftwareDevelopmentKitRunner.Execute;
    end
    else
      MessageBox(0, PChar(MSYSShellNotFound), PChar(ErrorTitle), MB_ICONERROR);
  finally
    DreamcastSoftwareDevelopmentKitRunner.Free;
  end;
end.

