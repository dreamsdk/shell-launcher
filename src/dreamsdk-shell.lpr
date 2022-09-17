program DreamSdkShell;

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
  Interfaces,
  Forms,
  Runner,
  SysTools,
  VerIntf;

var
  ShellLauncher: TDreamcastSoftwareDevelopmentKitRunner;

function GetWorkingDirectory: TFileName;
begin
  Result := EmptyStr;
  if ParamCount > 0 then
    Result := ParamStr(1);
end;

begin
{$IFDEF DEBUG}
  DebugLog('Starting ' + Application.Title + ' ...');
{$ENDIF}
  if not IsGetModuleVersionCommand then
  begin
    ShellLauncher := TDreamcastSoftwareDevelopmentKitRunner.Create;
    with ShellLauncher do
      try
        InteractiveShell := True;
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
  DebugLog('Exiting ' + Application.Title + ' ...');
{$ENDIF}
end.

