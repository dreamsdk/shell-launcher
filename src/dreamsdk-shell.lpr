program DreamSdkShell;

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
  Runner,
  SysTools,
  VerIntf;

var
  DreamcastSoftwareDevelopmentKitRunner: TDreamcastSoftwareDevelopmentKitRunner;

function GetWorkingDirectory: TFileName;
begin
  Result := EmptyStr;
  if ParamCount > 0 then
    Result := ParamStr(1);
end;

begin
{$IFDEF DEBUG}
  DebugLog('Starting...');
{$ENDIF}
  if not IsGetModuleVersionCommand then
  begin
    DreamcastSoftwareDevelopmentKitRunner := TDreamcastSoftwareDevelopmentKitRunner.Create;
    with DreamcastSoftwareDevelopmentKitRunner do
      try
        if CheckHealty then
        begin
          WorkingDirectory := GetWorkingDirectory;
          StartShell;
        end;
      finally
        Free;
      end;
  end
  else
    SaveModuleVersion;
{$IFDEF DEBUG}
  DebugLog('Exiting...');
{$ENDIF}
end.

