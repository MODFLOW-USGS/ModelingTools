unit frmImportModflow6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, frameGridUnit,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvToolEdit, Vcl.ExtCtrls, Vcl.Buttons,
  System.IOUtils, System.UITypes, Vcl.Grids, Vcl.ComCtrls;

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
    stat1: TStatusBar;
    lbGeoRef: TLabel;
    fedGeoRef: TJvFilenameEdit;
    procedure frameTransportNameFilesGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure frameTransportNameFilesGridExit(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure Initialize;
    procedure UpdateStatusBar(Sender: TObject; Text: string);
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

//var
//  frmImportModflow6: TfrmImportModflow6;

implementation

uses
  Modflow6ModelImporter, frmShowHideObjectsUnit, frmDataSetsUnits;

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
  if not TFile.Exists(Trim(edFlowSimFile.FileName)) then
  begin
    Beep;
    MessageDlg('You must select a simulation name file for flow to import a MODFLOW 6 model.', mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;

  if (Trim(fedGeoRef.FileName) <> '') and not TFile.Exists(Trim(fedGeoRef.FileName)) then
  begin
    Beep;
    MessageDlg('You specified a usgs.model.reference file that does not exist. Either don''t specify one or specify a file that exists.', mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;

  NameFiles := TStringList.Create;
  try
    NameFiles.Add(Trim(edFlowSimFile.FileName));

    for FileIndex := 1 to frameTransportNameFiles.seNumber.AsInteger do
    begin
      FileName := Trim(frameTransportNameFiles.Grid.Cells[0,FileIndex]);
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

    FreeAndNil(frmShowHideObjects);
    FreeAndNil(frmDataSets);

    Screen.Cursor := crHourGlass;
    Importer := TModflow6Importer.Create;
    ErrorMessages := TStringList.Create;
    try
      Importer.OnUpdateStatusBar := UpdateStatusBar;
      Importer.ImportModflow6Model(NameFiles, ErrorMessages, fedGeoRef.FileName);
    finally
      Importer.Free;
      ErrorMessages.Free;
      Screen.Cursor := crDefault;
    end;
  finally
    NameFiles.Free;
  end;
end;

procedure TfrmImportModflow6.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := 0;
end;

procedure TfrmImportModflow6.FormActivate(Sender: TObject);
begin
  inherited;
//  frameTransportNameFiles.Grid.Options := frameTransportNameFiles.Grid.Options + [goAlwaysShowEditor];
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
    frameTransportNameFiles.Grid.Cells[ACol, ARow] := Trim(odSimFiles.Files.Text);
  end;
end;

procedure TfrmImportModflow6.frameTransportNameFilesGridExit(Sender: TObject);
begin
  inherited;
  if (frameTransportNameFiles.seNumber.AsInteger = 1)
    and (frameTransportNameFiles.Grid.Cells[0,1] = '') then
  begin
    frameTransportNameFiles.seNumber.AsInteger := 0;
  end
  else
  begin
    frameTransportNameFiles.seNumber.AsInteger :=
      frameTransportNameFiles.Grid.RowCount -1;
  end;
end;

procedure TfrmImportModflow6.Initialize;
begin
  frameTransportNameFiles.Grid.Cells[0,0] := 'MODFLOW 6 Simulation Name Files for transport (optional)';
end;

procedure TfrmImportModflow6.UpdateStatusBar(Sender: TObject; Text: string);
begin
  stat1.SimpleText := Text;
  Application.ProcessMessages;
end;

end.
