unit IniFileUtilities;

interface

uses Windows, ShlObj, SysUtils, Classes;

function IniFileName(Handle: HWnd; ExeName: string): string;

implementation

const
  CSIDL_COMMON_APPDATA = $0023;
  // CSIDL_APPDATA replaces "All Users" with the user's logon name.
  CSIDL_APPDATA = $001A;

function AppDataFolderPath(Handle: HWnd): string;
Var
  S :String;
  recIDL : TItemIDList;
  ppIDL : PItemIDList;
  Res :Integer;
begin
  result := '';
  ppIDL := Addr( recIDL );
  Res := SHGetSpecialFolderLocation( Handle, CSIDL_APPDATA, ppIDL );
  If Res = NO_ERROR Then
  Begin
    SetLength( S, MAX_PATH );
    If SHGetPathFromIDList( ppIDL, PChar(S)) Then
    begin
      result := String( PChar(S) );
    end
    else
    begin
      RaiseLastOSError;
    end
  End
  else
  begin
    RaiseLastOSError;
  end;
end;
function GetAppDirectory(Handle: HWnd; ProgramName: string): string;
begin
  ProgramName := ChangeFileExt(ProgramName, '');
  ProgramName := ExtractFileName(ProgramName);
  result := AppDataFolderPath(Handle) + '\WRDAPP\' + ProgramName;
end;

procedure CreateDirectoryAndParents(DirName: string);
var
  Parents: TStringList;
  Index: integer;
  function ParentDir(const DirName: string): string;
  var
    Index: integer;
  begin
    result := '';
    for Index := Length(DirName) downto 1 do
    begin
      if DirName[Index] = '\' then
      begin
        result := Copy(DirName, 1, Index-1);
        Exit;
      end;
    end;
  end;
begin
  Parents := TStringList.Create;
  try
    while (not DirectoryExists(DirName)) and (DirName <> '') do
    begin
      Parents.Add(DirName);
      DirName := ParentDir(DirName);
    end;
    for Index := Parents.Count -1 downto 0 do
    begin
      if not CreateDir(Parents[Index]) then RaiseLastOSError;
    end;
  finally
    Parents.Free;
  end;

end;

function IniFileName(Handle: HWnd; ExeName: string): string;
var
  AppDir: string;
begin
  AppDir := GetAppDirectory(Handle, ExeName);
  if not DirectoryExists(AppDir) then
  begin
    CreateDirectoryAndParents(AppDir);
  end;
  result := ExtractFileName(ChangeFileExt(ExeName, '.ini'));
  result := AppDir + '\' + result;
end;

end.
