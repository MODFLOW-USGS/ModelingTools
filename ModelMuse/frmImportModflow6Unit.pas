unit frmImportModflow6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, frameGridUnit,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvToolEdit, Vcl.ExtCtrls, Vcl.Buttons,
  System.IOUtils, System.UITypes;

type
  TfrmImportModflow6 = class(TfrmCustomGoPhast)
    edFlowSimFile: TJvFilenameEdit;
    lblFlowSimFile: TLabel;
    frameTransportNameFiles: TframeGrid;
    Panel1: TPanel;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    odSimFiles: TOpenDialog;
    procedure frameTransportNameFilesGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    procedure Initialize;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  Modflow6ModelImporter;

{$R *.dfm}

{ TfrmImportModflow6 }

procedure TfrmImportModflow6.btnOKClick(Sender: TObject);
var
  NameFiles: TStringList;
  FileIndex: Integer;
  FileName: string;
  Importer: TModflow6Importer;
  ErrorMessages: TStringList;
begin
  inherited;
  if not TFile.Exists(edFlowSimFile.FileName) then
  begin
    Beep;
    MessageDlg('You must select a simulation name file for flow to import a MODFLOW 6 model.', mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;
  NameFiles := TStringList.Create;
  try
    NameFiles.Add(edFlowSimFile.FileName);

    for FileIndex := 1 to frameTransportNameFiles.seNumber.AsInteger do
    begin
      FileName := frameTransportNameFiles.Grid.Cells[0,FileIndex];
      if TFile.Exists(FileName) then
      begin
        NameFiles.Add(FileName);
      end
      else
      begin
        MessageDlg(Format('The file "%s" does not exist.', [FileName]), mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      end;
    end;

    Importer := TModflow6Importer.Create;
    ErrorMessages := TStringList.Create;
    try
      Importer.ImportModflow6Model(NameFiles, ErrorMessages);
    finally
      Importer.Free;
      ErrorMessages.Free;
    end;
  finally
    NameFiles.Free;
  end;
end;

procedure TfrmImportModflow6.FormCreate(Sender: TObject);
begin
  inherited;
  Initialize;
end;

procedure TfrmImportModflow6.frameTransportNameFilesGridButtonClick(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  if odSimFiles.Execute then
  begin
    frameTransportNameFiles.Grid.Cells[ACol, ARow] := odSimFiles.Files.Text;
  end;
end;

procedure TfrmImportModflow6.Initialize;
begin
  frameTransportNameFiles.Grid.Cells[0,0] := 'MODFLOW 6 Simulation Name Files for transport (optional)';
end;

end.
