unit WebUtils;

interface

uses
{$IFDEF LINUX}
Libc,
{$ENDIF}
{$IFDEF WIN32}
   Windows, ShellAPI, DdeMan, Dialogs,
{$ENDIF}

Classes;


{$IFDEF LINUX}
{ Browser just needs to be the name of the web browser.  The full path isn't
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
}


function LaunchURL(Browser: string; const URL: string): pid_t;
{$ENDIF}

{$IFDEF WIN32}
// BrowserPath should either be the full path of the web browser or
// it should be '' to use the default web browser.
function LaunchURL(var BrowserPath: string; const URL: string;
  Maximize: boolean = True) : boolean;


function DefaultBrowserPath: string;

{
  The call to the method must be accomplished with
  'IExplore’ for the Internet Explorer,
  'Netscape’ for the Netscape Communicator and
  'Opera’ for the Opera.
}
procedure CloseBrowser(BrowserName:string);
{$ENDIF}

function FindFiles(const Path, Mask: string; IncludeSubDir: boolean;
  const Files: TStringList): integer;

function FileToURL(const FileName: string): string;
  
implementation

uses SysUtils, StrUtils;

function FileToURL(const FileName: string): string;
var
  WrongCharPos: integer;
begin
  result := 'file:///' +  FileName;
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

{$IFDEF LINUX}
function LaunchURL(Browser: string; const URL: string): pid_t;
var
  open_max: Longint;
  i: Longint;

begin
  Result := fork;
  case Result of
  -1: begin
        raise Exception.Create('fork failed');
      end;

   0: begin
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
var
  FN, DN, RES: array[0..MAX_PATH] of char;
begin
  StrPCopy(FN, DOC);
  DN[0]  := #0;
  RES[0] := #0;
  FindExecutable(FN, DN, RES);
  Result := StrPas(RES);
end;

function GetTempFile(const Extension: string): string;
var
  Buffer: array[0..MAX_PATH] of char;
  aFile: string;
begin
  GetTempPath(SizeOf(Buffer) - 1, Buffer);
  GetTempFileName(Buffer, 'TMP', 0, Buffer);
  SetString(aFile, Buffer, StrLen(Buffer));
  Result := ChangeFileExt(aFile, Extension);
end;

function DefaultBrowserPath: string;
var
  temp: string;
  f: System.Text;
begin
  result := '';
  try
    // get a unique temporary file name
    // eine eindeutige Temporäre Datei bekommen
    temp := GetTempFile('.htm');
    // Create the file
    // Datei erstellen
    AssignFile(f, temp);
    try
      rewrite(f);
    finally
      closefile(f);
    end;
    // Show the path to the browser
    // Pfad + Programmname zum Browser anzeigen.
    result := GetAppName(temp);
    // Finally delete the temporary file
    // Temporaäre Datei wieder löschen
  finally
    Erase(f);
  end;
end;


function LaunchURL(var BrowserPath: string; const URL: string;
  Maximize: boolean = True) : boolean;
begin
   Result := True;
   if BrowserPath = '' then
   begin
     BrowserPath := DefaultBrowserPath;
   end;
   if BrowserPath = '' then
   begin
     Result := False;
     Exit;
   end;
   // Strip off quotes if present.
   if Pos('"', BrowserPath) >= 1 then
   begin
     BrowserPath := Copy(BrowserPath, Pos('"', BrowserPath) + 1, MAXINT) ;
   end;
   if Pos('"', BrowserPath) >= 1 then
   begin
     BrowserPath := Copy(BrowserPath, 1, Pos('"', BrowserPath) - 1) ;
   end;

   if not FileExists(BrowserPath) then
   begin
     Beep;
     MessageDlg('The location of the web browser has not been specified '
       + 'correctly.  The location that was specified was "'
       + BrowserPath + '".', mtWarning, [mbOK], 0);
     Exit;
   end;

   if Maximize then
   begin
     ShellExecute(0, 'open', PChar(BrowserPath), PChar(URL), nil,
       SW_SHOWMAXIMIZED) ;
   end
   else
   begin
     ShellExecute(0, 'open', PChar(BrowserPath), PChar(URL), nil, SW_SHOW) ;
   end;
end;

procedure CloseBrowser(BrowserName:string);
var
  DDE:TDDEClientConv;
begin
  DDE:=TDDEClientConv.Create(nil);
  Try

  // close the last instance of Browser.

    if DDE.SetLink(BrowserName,'WWW_Exit') then
       DDE.PokeData('anything' ,'anything')
  {        if DDE.PokeData('anything' ,'anything') then
              ShowMessage('Browser closed'); }

  // close all instances of Browser
  {      While DDE.SetLink(BrowserName,'WWW_Exit') do
           DDE.PokeData('anything' ,'anything') }
  {        if DDE.PokeData('anything' ,'anything') then
              ShowMessage('Browser was closed'); //This line is not necessary}
  Finally
    DDE.Free;
  End;
end;
{$ENDIF}

function FindFiles(const Path, Mask: string; IncludeSubDir: boolean;
  const Files: TStringList): integer;
var
  FindResult: integer;
  SearchRec : TSearchRec;
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
        FindFiles (Path + SearchRec.Name + PathDelim, Mask, TRUE, Files);
    end;

    FindResult := FindNext(SearchRec);
  end;
  { free memory }
  FindClose(SearchRec);
end;



end.
