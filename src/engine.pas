unit Engine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Settings;

type

  { TDreamcastSoftwareDevelopmentKitRunner }

  TDreamcastSoftwareDevelopmentKitRunner = class(TObject)
  private
    fExecutableMinTTY: TFileName;
    fExecutableShell: TFileName;
    fEnvironmentVariables: TStringList;
    fCommandLine: string;
    fSettings: TDreamcastSoftwareDevelopmentSettings;
    procedure InitializeEnvironment;
    function GetHealthy: Boolean;
    procedure RetrieveEnvironmentVariables;
    procedure SetShellWindowTitle(Title: string);
  protected
    property Settings: TDreamcastSoftwareDevelopmentSettings
      read fSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property CommandLine: string read fCommandLine write fCommandLine;
    property Healthy: Boolean read GetHealthy;
  end;

implementation

uses
  SysTools,
{$IFDEF Windows}
  Windows,
{$ENDIF}
  Process
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process
{$ENDIF}
  ;

resourcestring
  ShellApplicationWindowTitle   = 'DreamSDK Shell Launcher';

{ TDreamcastSoftwareDevelopmentKitRunner }

procedure TDreamcastSoftwareDevelopmentKitRunner.InitializeEnvironment;
const
  BINARY_DIRECTORY = 'msys\1.0\bin\';

begin
  fExecutableMinTTY := Settings.InstallPath + BINARY_DIRECTORY + 'mintty.exe';
  fExecutableShell := Settings.InstallPath + BINARY_DIRECTORY + 'sh.exe';
end;

function TDreamcastSoftwareDevelopmentKitRunner.GetHealthy: Boolean;
begin
  Result := FileExists(fExecutableShell)
    and FileExists(fExecutableMinTTY);
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.RetrieveEnvironmentVariables;
var
  i: Integer;

begin
  for i := 1 to GetEnvironmentVariableCount do
    fEnvironmentVariables.Add(GetEnvironmentString(i));
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.Execute;
var
  IsSingleShot: Boolean;
  OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};

begin
  RetrieveEnvironmentVariables;
  IsSingleShot := (Length(CommandLine) > 0);
  OurProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  try
    // Initialize Environment context
    OurProcess.Environment.AddStrings(fEnvironmentVariables);

    // Extracted from msys.bat
    if Settings.UseMinTTY and not IsSingleShot then
    begin
      OurProcess.Executable := fExecutableMinTTY;
      OurProcess.Parameters.Add('/bin/bash');
      OurProcess.Parameters.Add('-l');
    end
    else
    begin
      OurProcess.Executable := fExecutableShell;
      OurProcess.Parameters.Add('--login');
      OurProcess.Parameters.Add('-i');
    end;

    // Launch a single shot command from the shell
    if IsSingleShot then
      OurProcess.Environment.Add('_EXTERNAL_COMMAND=' + CommandLine);

    // Execute our process
    OurProcess.Execute;

    if IsSingleShot then
      SetShellWindowTitle(ShellApplicationWindowTitle);

  finally
    OurProcess.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.SetShellWindowTitle(Title: string);
var
  Tries: Integer;
  hWnd: THandle;
  FindTitle: string;

begin
  Tries := 0;

  FindTitle := fExecutableShell;
  repeat
    hWnd := FindWindow(nil, PChar(FindTitle));
    Sleep(10);
    Inc(Tries);
  until (hWnd <> 0) or (Tries > 50);

  if (hWnd <> 0) then
    SetWindowText(hWnd, PChar(Title));
end;

constructor TDreamcastSoftwareDevelopmentKitRunner.Create;
begin
  fEnvironmentVariables := TStringList.Create;
  fSettings := TDreamcastSoftwareDevelopmentSettings.Create;
  Settings.LoadConfiguration;
  InitializeEnvironment;
end;

destructor TDreamcastSoftwareDevelopmentKitRunner.Destroy;
begin
  fEnvironmentVariables.Free;
  fSettings.Free;
  inherited Destroy;
end;

end.

