{@abstract(@name provides a number of cross platform routines useful
  in accessing the internet.)}
unit RbwInternetUtilities;

interface

uses
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF WIN32}
  Windows, ShellAPI, DdeMan,
{$ENDIF}
{$IFDEF WIN64}
  Windows, ShellAPI, DdeMan,
{$ENDIF}
  SysUtils, Classes;


{ @abstract(@name launches a web browser with the specified URL.)

  @name is slightly different under Linux and Windows

  LINUX

  Browser just needs to be the name of the web browser.  The full path isn't
  required if the web browser is on the path. If no Browser is specified
  (Browser = ''), htmlview will be used which should start the default Browser.

  Any of the following should work.

  Launch('mozilla', 'http://www.kylix-patch.de.vu/');
  Launch('konqueror', 'http://www.kylix-patch.de.vu/');
  Launch('nautilus', 'http://www.kylix-patch.de.vu/');
  Launch('galeon', 'http://www.kylix-patch.de.vu/');
  Launch('netscape', 'http://www.kylix-patch.de.vu/');
  Launch('htmlview', 'http://www.kylix-patch.de.vu/');
  Launch('', 'http://www.kylix-patch.de.vu/');

  WINDOWS

  BrowserPath should either be the full path of the web browser or
  it should be '' to use the default web browser.

}

{$IFDEF LINUX}
function LaunchURL(Browser: string; const URL: string): pid_t;
{$ENDIF}

{$IFDEF WIN32}
function LaunchURL(var BrowserPath: string; const URL: string): boolean;

// @abstract(@name returns the path for the default browser.)
// @name only works under Windows.
function DefaultBrowserPath: string;

{ @abstract(@name is supposed to close a web browser but it is not reliable.)
  The call to the method must be accomplished with
  'IExplore' for the Internet Explorer,
  'Netscape' for the Netscape Communicator and
  'Opera' for the Opera.
}
procedure CloseBrowser(BrowserName: string);
{$ENDIF}

{$IFDEF WIN64}
function LaunchURL(var BrowserPath: string; const URL: string): boolean;

// @abstract(@name returns the path for the default browser.)
// @name only works under Windows.
function DefaultBrowserPath: string;

{ @abstract(@name is supposed to close a web browser but it is not reliable.)
  The call to the method must be accomplished with
  'IExplore' for the Internet Explorer,
  'Netscape' for the Netscape Communicator and
  'Opera' for the Opera.
}
procedure CloseBrowser(BrowserName: string);
{$ENDIF}

{@abstract(@name converts a local file name to a URL.)}
function FileNameToURL(const FileName: string): string;

// @abstract(@name finds files specified by Mask in the directory specified
// by path.)  The file names are added to Files.
// If IncludeSubDir is true, subdirectories of path will be searched too. 
function FindFiles(const Path, Mask: string; IncludeSubDir: boolean;
  const Files: TStringList): integer;

{
@name reads the file at the URL defined by URL_String and puts
it contents in Lines.
}
function ReadInternetFile(const URL_String: string;
  const Lines: TStringList; const AppName: string): boolean;
  
type
  EInternetConnectionError = class(Exception);

implementation

uses StrUtils, Wininet, Forms, IOUtils;

resourcestring
  StrForkFailed = 'fork failed';
  StrFailedToEstablish = 'Failed to establish an internet connection.';
  StrUnableToOpenInter = 'Unable to open Internet connection';
  StrUnableToOpenURL = 'Unable to open URL: %s';
  StrFailedToCloseURL = 'Failed to close URL';
  StrFailedToCloseInte = 'Failed to close Internet connection';

procedure DeleteIECacheEntry(const Url: string);
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: LongWord;
  dwEntrySize: LongWord;
  CachedUrl: string;
//  FoundCachedUrl: Boolean;
  AtPosition: integer;
  Dummy: Cardinal;
begin
// modified from http://www.delphitricks.com/source-code/internet/delete_the_temporary_internet_files.html
  dwEntrySize := 0;
  Dummy := FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  FindCloseUrlCache(Dummy);

  GetMem(lpEntryInfo, dwEntrySize);
  try
    if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
    hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);
    try
      if hCacheDir <> 0 then
      begin
//        FoundCachedUrl := False;
        repeat
          CachedUrl := string(lpEntryInfo^.lpszSourceUrlName);
          AtPosition := Pos('@', CachedUrl);
          if AtPosition >= 1 then
          begin
            CachedUrl := Copy(CachedUrl, AtPosition+1, MaxInt);
          end;
          if CachedUrl = Url then
          begin
            DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
            break;
          end;
          FreeMem(lpEntryInfo, dwEntrySize);
          dwEntrySize := 0;
          FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize);
          GetMem(lpEntryInfo, dwEntrySize);
          if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
        until not FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
      end;
    finally
      FindCloseUrlCache(hCacheDir);
    end;
  finally
    FreeMem(lpEntryInfo, dwEntrySize);
  end;
end;
{$IFDEF LINUX}

function LaunchURL(Browser: string; const URL: string): pid_t;
var
  open_max: Longint;
  i: Longint;

begin
  Result := fork;
  case Result of
    -1:
      begin
        raise Exception.Create(StrForkFailed);
      end;

    0:
      begin
        open_max := sysconf(_SC_OPEN_MAX);
        for i := Succ(STDERR_FILENO) to open_max do
        begin
          fcntl(i, F_SETFD, FD_CLOEXEC);
        end;
        if Browser = '' then
        begin
          // if no browser is specified, use the default browser.
          Browser := 'htmlview';
        end;

        execlp(PChar(Browser), PChar(Browser), PChar(URL), nil);
      end;
  end;
end;
{$ENDIF}

{$IFDEF WIN32}
function GetAppName(Doc: string): string;
{$ENDIF}
{$IFDEF WIN64}
function GetAppName(Doc: string): string;
{$ENDIF}
var
  FN, DN, RES: array[0..MAX_PATH] of char;
begin
  StrPCopy(FN, DOC);
  DN[0] := #0;
  RES[0] := #0;
  FindExecutable(FN, DN, RES);
  Result := StrPas(RES);
end;

{$IFDEF WIN32}
function GetTempFile(const Extension: string): string;
{$ENDIF}
{$IFDEF WIN64}
function GetTempFile(const Extension: string): string;
{$ENDIF}
var
//  Buffer: PChar;
//  Buffer: array[0..MAX_PATH] of char;
  aFile: string;
begin
  aFile := TPath.GetTempFileName;

//  GetMem(Buffer, SizeOf(Char) * (MAX_PATH+1));
//  GetTempPath(SizeOf(Buffer) - 1, Buffer);
//  GetTempFileName(Buffer, 'TMP', 0, Buffer);
////  SetString(aFile, Buffer, StrLen(Buffer));
////  StrPCopy
//  aFile := Buffer;
  Result := ChangeFileExt(aFile, Extension);
//  FreeMem(Buffer);
end;

{$IFDEF WIN32}
function DefaultBrowserPath: string;
{$ENDIF}
{$IFDEF WIN64}
function DefaultBrowserPath: string;
{$ENDIF}
var
  temp: string;
//  f: System.Text;
  AStringList: TStringList;
begin
  result := '';
  try
    // get a unique temporary file name
    // eine eindeutige Temporäre Datei bekommen
    temp := GetTempFile('.htm');
    // Create the file
    // Datei erstellen
    AStringList := TStringList.Create;
    try
      AStringList.SaveToFile(temp);
    finally
      AStringList.Free;
    end;
//    AssignFile(f, temp);
//    try
//      rewrite(f);
//    finally
//      closefile(f);
//    end;
    // Show the path to the browser
    // Pfad + Programmname zum Browser anzeigen.
    result := GetAppName(temp);
    // Finally delete the temporary file
    // Temporaäre Datei wieder löschen
  finally
    if FileExists(temp) then
    begin
      DeleteFile(temp);
//      Erase(f);
    end;
  end;
end;

{$IFDEF WIN32}
function LaunchURL(var BrowserPath: string; const URL: string): boolean;
{$ENDIF}
{$IFDEF WIN64}
function LaunchURL(var BrowserPath: string; const URL: string): boolean;
{$ENDIF}
begin
  Result := True;
  if BrowserPath = '' then
  begin
    BrowserPath := DefaultBrowserPath;
  end;
//  if BrowserPath = '' then
//  begin
//    Result := False;
//    ShowMessage('No default browser');
//    Exit;
//  end;
  // Strip off quotes if present.
  if Pos('"', BrowserPath) >= 1 then
  begin
    BrowserPath := Copy(BrowserPath, Pos('"', BrowserPath) + 1, MAXINT);
  end;
  if Pos('"', BrowserPath) >= 1 then
  begin
    BrowserPath := Copy(BrowserPath, 1, Pos('"', BrowserPath) - 1);
  end;

  if ExtractFileName(BrowserPath) = 'LaunchWinApp.exe' then
  begin
    BrowserPath := '';
  end;

  if BrowserPath = '' then
  begin
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_NORMAL);
  end
  else
  begin
    ShellExecute(0, 'open', PChar(BrowserPath), PChar(URL), nil, SW_SHOW);
  end;

end;

{$IFDEF WIN32}
procedure CloseBrowser(BrowserName: string);
{$ENDIF}
{$IFDEF WIN64}
procedure CloseBrowser(BrowserName: string);
{$ENDIF}
var
  DDE: TDDEClientConv;
begin
  DDE := TDDEClientConv.Create(nil);
  try

    // close the last instance of Browser.

    if DDE.SetLink(BrowserName, 'WWW_Exit') then
      DDE.PokeData('anything', 'anything')
        {        if DDE.PokeData('anything' ,'anything') then
                  ShowMessage('Browser closed'); }

      // close all instances of Browser
      {      While DDE.SetLink(BrowserName,'WWW_Exit') do
               DDE.PokeData('anything' ,'anything') }
      {        if DDE.PokeData('anything' ,'anything') then
                  ShowMessage('Browser was closed'); //This line is not necessary}
  finally
    DDE.Free;
  end;
end;

function FileNameToURL(const FileName: string): string;
var
  WrongCharPos : integer;
begin
  result := 'file:///' + FileName;

  WrongCharPos := Pos(' ', result);
  if WrongCharPos >= 1 then
  begin
    result := AnsiReplaceStr(result, ' ', '%20');
  end;

  WrongCharPos := Pos('\', result);
  if WrongCharPos >= 1 then
  begin
    result := AnsiReplaceStr(result, '\', '/');
  end;
end;

function FindFiles(const Path, Mask: string; IncludeSubDir: boolean;
  const Files: TStringList): integer;
var
  FindResult: integer;
  SearchRec: TSearchRec;
begin
  result := 0;

  FindResult := FindFirst(Path + Mask, faAnyFile - faDIRECTORY, SearchRec);
  while FindResult = 0 do
  begin
    { Add the full path to Files.}
    Files.Add(Path + SearchRec.Name);
    result := result + 1;

    FindResult := FindNext(SearchRec);
  end;
  { free memory }
  FindClose(SearchRec);

  if not IncludeSubDir then
    Exit;

{$IFDEF WIN32}
  // Find all directories on Windows.
  FindResult := FindFirst(Path + '*.*', faDIRECTORY, SearchRec);
{$ELSE}
{$IFDEF LINUX}
  // Find all directories on Linux.
  FindResult := FindFirst(Path + '*', faDIRECTORY, SearchRec);
{$ELSE}
  Assert(False);
{$ENDIF}
{$ENDIF}
  while FindResult = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
      and ((SearchRec.Attr and faDIRECTORY) = faDIRECTORY) then
    begin
      result := result +
        FindFiles(Path + SearchRec.Name + PathDelim, Mask, TRUE, Files);
    end;

    FindResult := FindNext(SearchRec);
  end;
  { free memory }
  FindClose(SearchRec);
end;

function ReadInternetFile(const URL_String: string;
  const Lines: TStringList; const AppName: string): boolean;
var
  Buffer: array[0..4095] of Char;
  Connection, URL: HINTERNET;
//  AppName: string;
  Stream: TMemoryStream;
  AmountRead: DWORD;
begin
  DeleteIECacheEntry(URL_String);

  result := False;
  if InternetAttemptConnect(0) <> ERROR_SUCCESS then
  begin
    raise EInternetConnectionError.Create(StrFailedToEstablish);
  end;

//  AppName := ExtractFileName(Application.Name);
  Connection := InternetOpen(PChar(AppName),
    INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Connection = nil then
  begin
    raise EInternetConnectionError.Create(StrUnableToOpenInter);
  end;

  try
    URL := InternetOpenUrl(Connection, PChar(URL_String), nil, 0,
      INTERNET_FLAG_PRAGMA_NOCACHE, 0);
    if URL = nil then
    begin
      raise EInternetConnectionError.Create(Format(StrUnableToOpenURL, [URL_String]));
    end;
    try
      Stream := TMemoryStream.Create;
      try
        AmountRead := 1;
        while AmountRead > 0 do
        begin
          if not InternetReadFile(URL, @Buffer, SizeOf(Buffer), AmountRead) then
          begin
            try
              RaiseLastOSError;
            except on E: EOSError do
              begin
                if (E.ErrorCode = 12001) // No more handles could be generated at this time.
                  or (E.ErrorCode = 12002) // The request has timed out.
                  then
                begin
                  result := False;
                  Exit;
                end
                else
                begin
                  raise
                end;
              end;
            end;
          end;
          Stream.Write(Buffer, AmountRead);
        end;

        Stream.Position := 0;
        Lines.LoadFromStream(Stream);
        result := True;
      finally
        Stream.Free;
      end;
    finally
      if not InternetCloseHandle(URL) then
      begin
        raise EInternetConnectionError.Create(StrFailedToCloseURL);
      end
    end;
  finally
    if not InternetCloseHandle(Connection) then
    begin
      raise EInternetConnectionError.Create(StrFailedToCloseInte);
    end
  end;
end;

end.
