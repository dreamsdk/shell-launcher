unit Engine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDreamcastSoftwareDevelopmentKitRunner }

  TDreamcastSoftwareDevelopmentKitRunner = class(TObject)
  private
    fApplicationPath: TFileName;
    fCommandLine: string;
    fMSYSExecutable: string;
    fUseMintty: Boolean;
    function GetApplicationPath: TFileName;
    function GetConfigurationFileName: TFileName;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure SetShellTitle(ProcessId: LongWord; Title: string);
    procedure SetShellTitleForSh(Title: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property CommandLine: string
      read fCommandLine write fCommandLine;
    property MSYSExecutable: string
      read fMSYSExecutable write fMSYSExecutable;
    property UseMintty: Boolean
      read fUseMintty write fUseMintty;
  end;

implementation

uses
  IniFiles,
{$IFDEF Windows}
  Windows,
{$ENDIF}
  Process
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process
{$ENDIF}
  , Utils;

const
  SH_BINARY_PATH            = 'bin\sh.exe';
  SH_ONE_SHOT_WINDOW_TITLE  = 'DreamSDK Shell';

{ TDreamcastSoftwareDevelopmentKitRunner }

function TDreamcastSoftwareDevelopmentKitRunner.GetApplicationPath: TFileName;
var
  Path: TFileName;
{$IFDEF Darwin}
  i: Integer;
{$ENDIF}

begin
  if (fApplicationPath = '') then
  begin
    Path := ExtractFilePath(ParamStr(0));
{$IFDEF Darwin}
    i := Pos('.app', Path);
    if i > 0 then
    begin
      i := LastDelimiter('/', Copy(Path, 1, i));
      Path := Copy(Path, 1, i);
    end;
{$ENDIF}
    fApplicationPath := IncludeTrailingPathDelimiter(Path);
  end;
  Result := fApplicationPath;
end;

function TDreamcastSoftwareDevelopmentKitRunner.GetConfigurationFileName: TFileName;
begin
  Result := GetApplicationPath +
    ChangeFileExt(ExtractFileName(ParamStr(0)), '.conf');
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.LoadConfig;
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    fMSYSExecutable := IniFile.ReadString('General', 'MSYSExecutable', ExpandFileName(GetApplicationPath + '..\..\msys.bat'));
    fUseMintty := IniFile.ReadBool('General', 'UseMintty', False);
  finally
    IniFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.Execute;
var
  IsSingleShot: Boolean;
{$IFDEF Windows}
  OurProcess: TProcess;
{$ELSE}
  OurProcess: TProcessUTF8;
{$ENDIF}

begin
  IsSingleShot := Length(CommandLine) > 0;

{$IFDEF Windows}
    OurProcess := TProcess.Create(nil);
{$ELSE}
    OurProcess := TProcessUTF8.Create(nil);
{$ENDIF}
    try
      OurProcess.Executable := MSYSExecutable;

      if FileExists(OurProcess.Executable) then
      begin
        // Use MinTTY instead of classical Windows Prompt
        if (UseMintty) then
        begin
          OurProcess.Parameters.Add('--mintty');
        end;

        // Launch a single shot command from MSYS Shell
        if IsSingleShot then
        begin
          OurProcess.Environment.Add('_EXTERNAL_COMMAND=' + CommandLine);
        end;

{$IFDEF DEBUG}
        ShowMessage(OurProcess.Parameters.Text);
{$ENDIF}

        if IsSingleShot then
          OurProcess.ShowWindow := swoHide;

        // Execute process
        OurProcess.Execute;

        // Cosmetic fix: Change the Console title if Windows Prompt
{$IFDEF Windows}
        if (not UseMintty) and IsSingleShot then
        begin
          SetShellTitleForSH(SH_ONE_SHOT_WINDOW_TITLE);
        end;
{$ENDIF}
      end;

    finally
      OurProcess.Free;
    end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.SaveConfig;
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    IniFile.WriteString('General', 'MSYSExecutable', fMSYSExecutable);
    IniFile.WriteBool('General', 'UseMintty', fUseMintty);
  finally
    IniFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.SetShellTitle(
  ProcessId: LongWord; Title: string);
var
  Tries: Integer;
  hWnd: THandle;

begin
  Tries := 0;

  repeat
    hWnd := FindWindowByProcessId(ProcessId);
    Sleep(100);
    Inc(Tries);
  until (hWnd <> 0) or (Tries > 50);

  if (hWnd <> 0) then
    SetWindowText(hWnd, PChar(Title));
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.SetShellTitleForSh(
  Title: string);
var
  Tries: Integer;
  hWnd: THandle;
  FindTitle: string;

begin
  Tries := 0;

  FindTitle := IncludeTrailingPathDelimiter(ExtractFilePath(MSYSExecutable)) + SH_BINARY_PATH;
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
  LoadConfig;
end;

destructor TDreamcastSoftwareDevelopmentKitRunner.Destroy;
begin
  SaveConfig;
  inherited Destroy;
end;

end.

