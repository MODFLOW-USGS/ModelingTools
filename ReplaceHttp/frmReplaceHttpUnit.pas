unit frmReplaceHttpUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmReplaceHttpWithHttps = class(TForm)
    btnReplaceHttpWithHttps: TButton;
    procedure btnReplaceHttpWithHttpsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ReplaceHttpInDirectory(Directory: string);
    procedure ReplaceInFilteredFiles(Filter: string; Directory: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmReplaceHttpWithHttps: TfrmReplaceHttpWithHttps;

implementation

uses
  VCL.FileCtrl, System.IOUtils, System.Types;

{$R *.dfm}

procedure TfrmReplaceHttpWithHttps.btnReplaceHttpWithHttpsClick(Sender: TObject);
var
  Directory: string;
begin
  if SelectDirectory('Select Root directory', '', Directory,
    [sdValidateDir], self)  then
  begin
    ReplaceHttpInDirectory(Directory);
    ShowMessage('Done');
  end;
end;

procedure TfrmReplaceHttpWithHttps.FormCreate(Sender: TObject);
var
  ADirectory: string;
  DirIndex: Integer;
begin
  if ParamCount > 0 then
  begin
    for DirIndex := 1 to ParamCount do
    begin
      ADirectory := ParamStr(DirIndex);
      if TDirectory.Exists(ADirectory) then
      begin
        ReplaceHttpInDirectory(ADirectory);
      end
      else
      begin
        MessageDlg(Format('%s does not exist.', [ADirectory]), mtWarning, [mbOK], 0);
      end;
    end;
    Application.Terminate;
  end;
end;

procedure TfrmReplaceHttpWithHttps.ReplaceHttpInDirectory(Directory: string);
var
  Filter: string;
begin
  ReplaceInFilteredFiles('*.htm*', Directory);
  ReplaceInFilteredFiles('*.js', Directory);
end;

procedure TfrmReplaceHttpWithHttps.ReplaceInFilteredFiles(Filter: string; Directory: string);
var
  FileArray: TStringDynArray;
  FileIndex: Integer;
  AFileName: string;
  AFile: TStringList;
  Changed: Boolean;
  LineIndex: Integer;
  ALine: string;
begin
  FileArray := TDirectory.GetFiles(Directory, Filter, TSearchOption.soAllDirectories);
  for FileIndex := 0 to Length(FileArray) - 1 do
  begin
    AFileName := FileArray[FileIndex];
    AFile := TStringList.Create;
    try
      AFile.LoadFromFile(AFileName);
      Changed := False;
      for LineIndex := 0 to AFile.Count - 1 do
      begin
        ALine := AFile[LineIndex];
        if ALine.Contains('http://') then
        begin
          ALine := StringReplace(ALine, 'http://', 'https://', [rfReplaceAll]);
          AFile[LineIndex] := ALine;
          Changed := True;
        end;
      end;
      if Changed then
      begin
        AFile.SaveToFile(AFileName);
      end;
    finally
      AFile.Free;
    end;
  end;
end;

end.
