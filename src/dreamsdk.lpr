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
  Version,
  SysTools,
  VerIntf,
  Runner;

var
  DreamcastSoftwareDevelopmentKitRunner: TDreamcastSoftwareDevelopmentKitRunner;

begin
  if not IsGetModuleVersionCommand then
  begin
    DreamcastSoftwareDevelopmentKitRunner := TDreamcastSoftwareDevelopmentKitRunner.Create;
    with DreamcastSoftwareDevelopmentKitRunner do
      try
        if CheckHealty then
          StartShell
      finally
        Free;
      end;
  end
  else
    SaveModuleVersion;
end.

