program DCSDK;

{$mode objfpc}{$H+}
{$R *.res}

uses
  Interfaces,
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Classes,
  Engine,
  Version,
  SysTools,
  VerIntf;

resourcestring
  MSYSShellNotFound = 'MinGW/MSYS is not properly installed.';
  ErrorTitle = 'Error';

var
  DreamcastSoftwareDevelopmentKitRunner: TDreamcastSoftwareDevelopmentKitRunner;
  IsGetVersion: Boolean = False;
  CommandLine: string;

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
    if LowerCase(Param) <> GET_MODULE_VERSION_SWITCH then
    begin
      Result := Result + Sep + Param;
      Sep := ' ';
    end
    else
      IsGetVersion := True;
  end;
end;

begin
  CommandLine := ParseCommandLine;
  if not IsGetVersion then
  begin
    DreamcastSoftwareDevelopmentKitRunner := TDreamcastSoftwareDevelopmentKitRunner.Create;
    try
      if DreamcastSoftwareDevelopmentKitRunner.Healthy then
      begin
        DreamcastSoftwareDevelopmentKitRunner.CommandLine := CommandLine;
        DreamcastSoftwareDevelopmentKitRunner.Execute;
      end
      else
        MessageBox(0, PChar(MSYSShellNotFound), PChar(ErrorTitle), MB_ICONERROR);
    finally
      DreamcastSoftwareDevelopmentKitRunner.Free;
    end;
  end
  else
    SaveModuleVersion(ParamStr(0), GetProcessID);
end.

