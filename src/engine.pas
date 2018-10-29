unit Engine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDreamcastSoftwareDevelopmentKitRunner }

  TDreamcastSoftwareDevelopmentKitRunner = class(TObject)
  private
    fEnvironmentVariables: TStringList;
    fAutomatedCall: Boolean;
    fInstallPath: TFileName;
    fExecutableMinTTY: TFileName;
    fExecutableSH: TFileName;
    fApplicationPath: TFileName;
    fCommandLine: string;
    fUseMinTTY: Boolean;
    function GetHealthy: Boolean;
    function GetApplicationPath: TFileName;
    function GetConfigurationFileName: TFileName;
    procedure RetrieveEnvironmentVariables;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure SetShellWindowTitle(Title: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property AutomatedCall: Boolean read fAutomatedCall write fAutomatedCall;
    property CommandLine: string read fCommandLine write fCommandLine;
    property UseMinTTY: Boolean read fUseMinTTY write fUseMinTTY;
    property Healthy: Boolean read GetHealthy;
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
  ;

resourcestring
  ShellApplicationWindowTitle   = 'DreamSDK Shell Launcher';

{ TDreamcastSoftwareDevelopmentKitRunner }

function TDreamcastSoftwareDevelopmentKitRunner.GetHealthy: Boolean;
begin
  Result := FileExists(fExecutableSH) and FileExists(fExecutableMinTTY);
end;

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
  Result := GetApplicationPath + 'dcsdk.conf';
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.RetrieveEnvironmentVariables;
var
  i: Integer;

begin
  for i := 1 to GetEnvironmentVariableCount do
    fEnvironmentVariables.Add(GetEnvironmentString(i));
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.LoadConfig;
var
  IniFile: TIniFile;
  DefaultInstallationPath, MSYSBase: TFileName;

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    fUseMinTTY := IniFile.ReadBool('General', 'UseMinTTY', False);

    DefaultInstallationPath := ExpandFileName(GetApplicationPath + '..\..\..\..\');
    fInstallPath := IncludeTrailingPathDelimiter(IniFile.ReadString('General',
      'InstallPath', DefaultInstallationPath));

    MSYSBase := fInstallPath + 'msys\1.0\';
    fExecutableMinTTY := MSYSBase + 'bin\mintty.exe';
    fExecutableSH := MSYSBase + 'bin\sh.exe';
  finally
    IniFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.Execute;
var
  IsSingleShot: Boolean;
  OutputResult: string;
  OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};

  function ProcessOutput: string;
  const
    READ_BYTES = 2048;

  var
    MemStream: TMemoryStream;
    NumBytes: LongInt;
    BytesRead: LongInt;
    OutputLines: TStringList;

  begin
    MemStream := TMemoryStream.Create;
    try
      BytesRead := 0;

      while OurProcess.Running do
      begin
        MemStream.SetSize(BytesRead + READ_BYTES);
        NumBytes := OurProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
        if NumBytes > 0 then
        begin
          Inc(BytesRead, NumBytes);
        end
        else
          Break;
      end;

      MemStream.SetSize(BytesRead);

      OutputLines := TStringList.Create;
      try
         OutputLines.LoadFromStream(MemStream);
         Result := OutputLines.Text;
      finally
        OutputLines.Free;
      end;

    finally
      MemStream.Free;
    end;
  end;

  procedure SaveOutput;
  var
    StringList: TStringList;

  begin
    StringList := TStringList.Create;
    try
      StringList.Text := OutputResult;
      StringList.SaveToFile(fApplicationPath + IntToStr(GetProcessID) + '.tmp');
    finally
      StringList.Free;
    end;
  end;

begin
  RetrieveEnvironmentVariables;

  IsSingleShot := Length(CommandLine) > 0;

{$IFDEF Windows}
    OurProcess := TProcess.Create(nil);
{$ELSE}
    OurProcess := TProcessUTF8.Create(nil);
{$ENDIF}
    try
      // Initialize Environment context
      OurProcess.Environment.AddStrings(fEnvironmentVariables);

      // Extracted from msys.bat
      if UseMinTTY and not IsSingleShot then
      begin
        OurProcess.Executable := fExecutableMinTTY;
        OurProcess.Parameters.Add('/bin/bash');
        OurProcess.Parameters.Add('-l');
      end
      else
      begin
        OurProcess.Executable := fExecutableSH;
        OurProcess.Parameters.Add('--login');
        OurProcess.Parameters.Add('-i');
      end;

      // Launch a single shot command from the shell
      if IsSingleShot then
        OurProcess.Environment.Add('_EXTERNAL_COMMAND=' + CommandLine);

      // If this instance is launcher through --dcsdk-automated-call switch
      if AutomatedCall then
      begin
        OurProcess.Environment.Add('_AUTOMATED_CALL=1');
        OurProcess.Options := [poUsePipes, poStderrToOutput];
        OurProcess.ShowWindow := swoHide;
      end;

      // Execute our process
      OurProcess.Execute;

      // If automated, then we will recover the output
      if AutomatedCall then
      begin
        OutputResult := ProcessOutput;
        SaveOutput;
      end
      else if IsSingleShot then
        // If not automated, change the Console title
        SetShellWindowTitle(ShellApplicationWindowTitle);

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
    IniFile.WriteString('General', 'InstallPath', fInstallPath);
    IniFile.WriteBool('General', 'UseMinTTY', fUseMintty);
  finally
    IniFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.SetShellWindowTitle(Title: string);
var
  Tries: Integer;
  hWnd: THandle;
  FindTitle: string;

begin
  Tries := 0;

  FindTitle := fExecutableSH;
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
  LoadConfig;
end;

destructor TDreamcastSoftwareDevelopmentKitRunner.Destroy;
begin
  fEnvironmentVariables.Free;
  SaveConfig;
  inherited Destroy;
end;

end.

