{@abstract(@name defines a cross-platform method of viewing context-sensitive
  help in html files.  The help is accessed by Keyword.)
  To use this help viewer, include it in the uses clause of the application.
  In the initialization section, the help viewer will look for a subdirectory
  named help in the directory that has the application.  It will search that
  directory for any files with the extensions .htm or .html.  The names of the
  web pages (without the extensions) are the keywords recognized by the help
  viewer.  When help is requested, it will launch the default web browser
  with the URL of the web page whose name matches the keyword. The match of
  keyword to file name is not case-sensitive even on file systems where file
  names are case sensitive.

  If all the web pages aren't present when the application starts,
  @link(UpdateHelpFiles) can be used to update the list of files recognized
  by the help system.

  It is possible to have the help viewer
  go to a specific target within a web page by adding "#" and the name of
  an anchor to the help keyword.  For example to go to AWebPage.html#ThisSpot,
  the keyword should be "AWebPage#ThisSpot".

  Nearly everything in @name is in the implementation section.}
unit RbwHelpViewer;

interface

// @name is used to update the list of files that the help viewer
// uses for showing help.  @name should be called if a new file has been
// created dynamically.  
procedure UpdateHelpFiles;

implementation

uses
{$IFDEF WIN32}
  WinHelpViewer,
{$ENDIF}
  SysUtils, StrUtils, Classes, HelpIntfs, QDialogs, QForms,
  RbwInternetUtilities;

resourcestring
  // This is the name of the help viewer.
  rsViwerName = 'RBW Html Help Viewer';

type
  // This defines the help viewer.
  THtmlHelpViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    FHelpManager: IHelpManager;
    // @name identifies this help viewer to the operating system.
    FViewerID: integer;
    // FHelpDirectory is the full path of the directory for help files.
    FHelpDirectory: string;
    // This is a list of the names of web pages used for help
    FFiles: TStringList;
    // InternalShutdown lets the FHelpManager know that it can free itself.
    procedure InternalShutdown;
    // This gets the names of the help files.
    procedure GetHelpFiles(Directory: string);
    procedure Release;
  public
    // Create creates an instance of THtmlHelpViewer and retrieves the names
    // of all the help files.
    constructor Create;
    destructor Destroy; override;
    {ICustomHelpViewer methods}
    function GetViewerName: string;
    function UnderstandsKeyword(const HelpString: string): integer;
    function GetHelpStrings(const HelpString: string): TStringList;
    function CanShowTableOfContents: boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: string);
    procedure NotifyID(const ViewerID: integer);
    procedure SoftShutDown;
    procedure ShutDown;
    constructor UpdateHelpFiles;
  end;

{$IFDEF WIN32}
  // TWinHelpDeactivator is used to ensure that THtmlHelpViewer
  // is used on Windows in preference to the default help viewer.
  TWinHelpDeactivator = class(TInterfacedObject, IWinHelpTester)
  public
    constructor Create;
    destructor Destroy; override;
    {IWinHelpTester methods}
    function CanShowALink(const ALink, FileName: string): Boolean;
    function CanShowTopic(const Topic, FileName: string): Boolean;
    function CanShowContext(const Context: Integer;
      const FileName: string): Boolean;
    function GetHelpStrings(const ALink: string): TStringList;
    function GetHelpPath: string;
    function GetDefaultHelpFile: string;
  end;
{$ENDIF}

var
  {HelpViewer is a global variable created in the initialization section
  and destroyed in the finalizaton section.}
  HelpViewer: THtmlHelpViewer;
{$IFDEF WIN32}
  WinHelpDeactivator: TWinHelpDeactivator;
{$ENDIF}

procedure UpdateHelpFiles;
begin
  HelpViewer.UpdateHelpFiles;
end;

  { THtmlHelpViewer }

function THtmlHelpViewer.CanShowTableOfContents: boolean;
begin
  result := False;
end;

constructor THtmlHelpViewer.UpdateHelpFiles;
begin
  FFiles.Free;
  FFiles := TStringList.Create;
  GetHelpFiles(FHelpDirectory);
end;

constructor THtmlHelpViewer.Create;
begin
  inherited Create;
  FFiles := TStringList.Create;
  FHelpDirectory := ExtractFileDir(Application.ExeName) + PathDelim + 'help' +
    PathDelim;
  GetHelpFiles(FHelpDirectory);
  HelpIntfs.RegisterViewer(self, FHelpManager);
end;

destructor THtmlHelpViewer.Destroy;
begin
  InternalShutDown;
  FFiles.Free;
  inherited;
end;

procedure THtmlHelpViewer.GetHelpFiles(Directory: string);
begin
  if DirectoryExists(FHelpDirectory) then
  begin
    // On Windows, this finds both *.htm and *.html files.
    // On Linux, this finds only *.htm files.
    FindFiles(Directory, '*.htm', True, FFiles);
{$IFDEF LINUX}
    // On Linux, this finds *.html files.
    FindFiles(Directory, '*.html', True, FFiles);
{$ENDIF}
  end;
end;

function THtmlHelpViewer.GetHelpStrings(
  const HelpString: string): TStringList;
begin
  result := TStringList.Create;
  result.Add(HelpString);
end;

function THtmlHelpViewer.GetViewerName: string;
begin
  result := rsViwerName;
end;

procedure THtmlHelpViewer.InternalShutdown;
begin
  if Assigned(FHelpManager) then
    FHelpManager.Release(FViewerID);
  ShutDown;
end;

procedure THtmlHelpViewer.NotifyID(const ViewerID: integer);
begin
  FViewerID := ViewerID;
end;

procedure THtmlHelpViewer.ShowHelp(const HelpString: string);
var
  Browser: string;
  URL: string;
  Index: integer;
  FileName: string;
  Key: string;
  Target: string;
  TargetPositon: integer;
begin
  if FFiles.Count > 0 then
  begin
    Key := LowerCase(HelpString);
    TargetPositon := Pos('#', HelpString);
    if TargetPositon >= 1 then
    begin
      Key := Copy(Key, 1, TargetPositon - 1);
      Target := Copy(HelpString, TargetPositon, MAXINT);
    end
    else
    begin
      Target := '';
    end;

    URL := FFiles[0];
    for Index := 0 to FFiles.Count - 1 do
    begin
      FileName := ExtractFileName(FFiles[Index]);
      FileName := ChangeFileExt(FileName, '');
      if LowerCase(FileName) = Key then
      begin
        URL := FFiles[Index];
        Break;
      end;
    end;

    URL := FileNameToURL(URL + Target);

    LaunchURL(Browser, URL);
  end;
end;

procedure THtmlHelpViewer.ShowTableOfContents;
begin
  raise EHelpSystemException.Create('Unable to show table of contents.');
end;

procedure THtmlHelpViewer.ShutDown;
begin
  if Assigned(FHelpManager) then
    FHelpManager := nil;
end;

procedure THtmlHelpViewer.SoftShutDown;
begin
  // nothing
end;

function THtmlHelpViewer.UnderstandsKeyword(
  const HelpString: string): integer;
begin
  result := 1;
end;

procedure THtmlHelpViewer.Release;
begin
  _Release;
end;

{$IFDEF WIN32}

{ TWinHelpDeactivator }

function TWinHelpDeactivator.CanShowALink(const ALink,
  FileName: string): Boolean;
begin
  result := False;
end;

function TWinHelpDeactivator.CanShowContext(const Context: Integer;
  const FileName: string): Boolean;
begin
  result := False;
end;

function TWinHelpDeactivator.CanShowTopic(const Topic,
  FileName: string): Boolean;
begin
  result := False;
end;

constructor TWinHelpDeactivator.Create;
begin
  inherited;
  WinHelpTester := self;
end;

destructor TWinHelpDeactivator.Destroy;
begin
  WinHelpTester := nil;
  inherited;
end;

function TWinHelpDeactivator.GetDefaultHelpFile: string;
begin
  result := '';
end;

function TWinHelpDeactivator.GetHelpPath: string;
begin
  result := '';
end;

function TWinHelpDeactivator.GetHelpStrings(
  const ALink: string): TStringList;
begin
  result := nil;
  Assert(False);
end;
{$ENDIF}

initialization
  if not Assigned(HelpViewer) then
  begin
    HelpViewer := THtmlHelpViewer.Create;
  end;
{$IFDEF WIN32}
  if not Assigned(WinHelpDeactivator) then
  begin
    WinHelpDeactivator := TWinHelpDeactivator.Create;
  end;
{$ENDIF}

finalization
  if Assigned(HelpViewer) then
  begin
    HelpViewer.Release;
  end;
{$IFDEF WIN32}
  if Assigned(WinHelpDeactivator) then
  begin
    WinHelpTester := nil;
  end;
{$ENDIF}
end.

