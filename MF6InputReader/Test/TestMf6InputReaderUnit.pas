unit TestMf6InputReaderUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvComponentBase, JvCreateProcess,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvToolEdit;

type
  TForm1 = class(TForm)
    Button1: TButton;
    JvCreateProcess1: TJvCreateProcess;
    fedMf6InputReader: TJvFilenameEdit;
    fedInputFiles: TJvFilenameEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Names: TStringList;
  FileIndex: Integer;
  FileName: string;
begin
  if not TFile.Exists(fedMf6InputReader.FileName) then
  begin
    Beep;
    MessageDlg('Specify the name of the MF6 input file reader', mtWarning, [mbOK], 0);
    Exit;
  end;
  if not TFile.Exists(fedInputFiles.FileName) then
  begin
    Beep;
    MessageDlg('Specify the name of the file containing the mfsim.nam files', mtWarning, [mbOK], 0);
    Exit;
  end;
  Names := TStringList.Create;
  try
    Names.LoadFromFile(fedInputFiles.FileName);
    for FileIndex := 0 to Names.Count - 1 do
    begin
      FileName := Names[FileIndex];
      SetCurrentDir(ExtractFileDir(FileName));
      JvCreateProcess1.CommandLine := fedMf6InputReader.FileName + ' ' + ExtractFileName(FileName);
      JvCreateProcess1.Run;
    end;
  finally
    Names.Free;
    Beep;
    ShowMessage('Done');
  end;
end;

end.
