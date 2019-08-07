// from either of the following:
// http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.rtl.win32&messageid=501_3f8aac4b@newsgroups.borland.com
// http://www.swissdelphicenter.ch/torry/showcode.php?id=261

unit forceforeground;

interface
function ForceForegroundWindow(hwnd: THandle): boolean;

implementation
uses windows, sysutils;

function ForceForegroundWindow(hwnd: THandle): boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID : DWORD;
  timeout : DWORD;


begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);
  if GetForegroundWindow = hwnd then Result := true
  else begin

// Windows 98/2000 doesn't want to foreground a window when some other
// window has keyboard focus

  if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4))
or
  ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
  ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
  (Win32MinorVersion > 0)))) then
   begin

// Code from Karl E. Peterson, www.mvps.org/vb/sample.htm
// Converted to Delphi by Ray Lischner
// Published in The Delphi Magazine 55, page 16

    Result := false;
    ForegroundThreadID :=
    GetWindowThreadProcessID(GetForegroundWindow,nil);
    ThisThreadID := GetWindowThreadPRocessId(hwnd,nil);
    if AttachThreadInput(ThisThreadID, ForegroundThreadID, true) then
    begin
    BringWindowToTop(hwnd); // IE 5.5 related hack
    SetForegroundWindow(hwnd);
    AttachThreadInput(ThisThreadID, ForegroundThreadID, false);
    Result := (GetForegroundWindow = hwnd);
    end;
    if not Result then begin

// Code by Daniel P. Stasinski

     SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
     SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
     SPIF_SENDCHANGE);
     BringWindowToTop(hwnd); // IE 5.5 related hack
     SetForegroundWindow(hWnd);
     SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0,
     TObject(timeout), SPIF_SENDCHANGE);
   end;
  end
  else
  begin
    BringWindowToTop(hwnd); // IE 5.5 related hack
    SetForegroundWindow(hwnd);
  end;

  Result := (GetForegroundWindow = hwnd);
  end;
end; { ForceForegroundWindow }


end.

