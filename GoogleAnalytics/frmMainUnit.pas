unit frmMainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, JvBaseDlg,
  JvSelectDirectory;

type
  TfrmMain = class(TForm)
    jvsd1: TJvSelectDirectory;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FUpdateJQueryFile: Boolean;
    procedure GetWebPages(Directory: string; FileList: TStringList);
    procedure ModifyWebPage(FileName: string);
    procedure ModifyWebPagesInDirectory(Directory: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.IOUtils, System.Types;

{$R *.dfm}
const
  JQueryOld = 'jquery-1.4.2.min.js';
  JQueryNew = 'jquery-1.11.1.js';

procedure TfrmMain.btn1Click(Sender: TObject);
begin
  if jvsd1.Execute then
  begin
    jvsd1.InitialDir := jvsd1.Directory;
    ModifyWebPagesInDirectory(jvsd1.Directory);
    ShowMessage('Done');
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ParamIndex: Integer;
  Directory: string;
begin
  if ParamCount > 0 then
  begin
    for ParamIndex := 1 to ParamCount do
    begin
      Directory := ParamStr(ParamIndex);
      if DirectoryExists(Directory) then
      begin
        ModifyWebPagesInDirectory(Directory);
      end
      else
      begin
        Beep;
        MessageDlg(Directory + ' does not exist.', mtError, [mbOK], 0);
      end;
    end;
    Application.Terminate;
  end;
end;

procedure TfrmMain.GetWebPages(Directory: string; FileList: TStringList);
var
  Files: TStringDynArray;
  Index: Integer;
begin
  FileList.BeginUpdate;
  try
    Files := TDirectory.GetFiles(Directory, '*.htm',
      TSearchOption.soAllDirectories);
    for Index := 0 to Length(Files) - 1 do
    begin
      FileList.Add(Files[Index]);
    end;

    Files := TDirectory.GetFiles(Directory, '*.html',
      TSearchOption.soAllDirectories);
    for Index := 0 to Length(Files) - 1 do
    begin
      FileList.Add(Files[Index]);
    end;
  finally
    FileList.EndUpdate;
  end;
end;

procedure TfrmMain.ModifyWebPage(FileName: string);
const
  GoogleAnalytics = '<!--#include virtual="/SSI/google_analytics_head.html"-->';
  InsertText = '    ' + GoogleAnalytics;
  EndHead = '</head>';
var
  ALine: string;
  CharIndex: Integer;
  LineIndex: Integer;
  BreakLine: Boolean;
  AWebPage: TStringList;
  EndHeadPos: Integer;
  LowerCaseLine: string;
  GoogleSearch: string;
  SaveFile: boolean;
  JQueryPos: Integer;
begin
  GoogleSearch := LowerCase(GoogleAnalytics);
  AWebPage := TStringList.Create;
  SaveFile := False;
  try
    AWebPage.LoadFromFile(FileName);
    for LineIndex := 0 to AWebPage.Count - 1 do
    begin
      ALine := AWebPage[LineIndex];
      LowerCaseLine := LowerCase(ALine);
      if Pos(GoogleSearch, LowerCaseLine) > 0 then
      begin
        Break;
      end;
      EndHeadPos := Pos(EndHead, LowerCaseLine);
      if EndHeadPos > 0 then
      begin
        BreakLine := False;
        for CharIndex := EndHeadPos-1 downto 1 do
        begin
          if (ALine[CharIndex] <> ' ') then
          begin
            BreakLine := True;
            Break;
          end;
        end;
        if BreakLine then
        begin
          ALine := Copy(ALine, 1, EndHeadPos - 1) + sLineBreak + GoogleAnalytics
            + sLineBreak + Copy(ALine, EndHeadPos, MaxInt);
          AWebPage[LineIndex] := ALine;
        end
        else
        begin
          AWebPage.Insert(LineIndex, InsertText);
        end;
        SaveFile := True;
//        AWebPage.SaveToFile(FileName);
        break;
//        Exit;
      end;
    end;
    for LineIndex := 0 to AWebPage.Count - 1 do
    begin
      ALine := AWebPage[LineIndex];
      JQueryPos := Pos(JQueryOld, ALine);
      if JQueryPos > 0 then
      begin
        ALine := Copy(ALine, 1, JQueryPos-1) + JQueryNew
          + Copy(ALine, JQueryPos+Length(JQueryOld), MAXINT);
        AWebPage[LineIndex] := ALine;
        SaveFile := True;
        FUpdateJQueryFile := True;
        break;
      end;
    end;
//      LowerCaseLine := LowerCase(ALine);
  finally
    if SaveFile then
    begin
      AWebPage.SaveToFile(FileName);
    end;
    AWebPage.Free;
  end;
end;

procedure TfrmMain.ModifyWebPagesInDirectory(Directory: string);
var
  FileIndex: Integer;
  FileName: string;
  FileList: TStringList;
  JQueryOldFileName: string;
  JQuerySourceFileName: string;
  JQueryNewFileName: string;
begin
  FUpdateJQueryFile := False;
  FileList := TStringList.Create;
  try
    GetWebPages(Directory, FileList);
    for FileIndex := 0 to FileList.Count - 1 do
    begin
      FileName := FileList[FileIndex];
      ModifyWebPage(FileName);
    end;
  finally
    FileList.Free;
  end;
  JQueryOldFileName := IncludeTrailingPathDelimiter(Directory) + JQueryOld;
  if FileExists(JQueryOldFileName) then
  begin
    DeleteFile(JQueryOldFileName);
  end;
//  if FUpdateJQueryFile then
  begin
    JQuerySourceFileName := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + JQueryNew;
    Assert(FileExists(JQuerySourceFileName));
    JQueryNewFileName := IncludeTrailingPathDelimiter(Directory) + JQueryNew;
    TFile.Copy(JQuerySourceFileName, JQueryNewFileName, True);
  end;
end;

end.
