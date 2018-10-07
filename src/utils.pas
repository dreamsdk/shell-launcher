unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function FindWindowByProcessId(pid: LongWord): THandle;

implementation

uses
  Windows;

function FindWindowByProcessId(pid: LongWord): THandle;
var
  hCurWnd: THandle;
  cur_pid: LongWord;

begin
	hCurWnd := GetTopWindow(0);

  while (hCurWnd <> 0) do
	begin
    cur_pid := 0;
		GetWindowThreadProcessId(hCurWnd, cur_pid);

		if (cur_pid = pid) then
		begin
			if (IsWindowVisible(hCurWnd)) then
			begin
        Result := hCurWnd;
        Exit;
			end;
		end;

		hCurWnd := GetNextWindow(hCurWnd, GW_HWNDNEXT);
	end;

	Result := 0;
end;

end.

