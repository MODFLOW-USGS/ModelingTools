unit frmEditMetadataUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, frameMetaDataEditorUnit;

type
  TfrmEditMetadata = class(TForm)
    pnl1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    frameMetaDataEd: TframeMetaDataEditor;
    tmrLoadFile: TTimer;
    btnSearch: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure tmrLoadFileTimer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure frameMetaDataEdDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure frameMetaDataEdDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FXmlFileName: string;
    { Private declarations }
  public
    procedure EditFile(const XmlFileName: string);
    { Public declarations }
  end;

var
  frmEditMetadata: TfrmEditMetadata;

implementation

uses
  System.IOUtils, frmModelArchiverUnit, frmSearchUnit;

{$R *.fmx}

{ TfrmEditMetadata }

procedure TfrmEditMetadata.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEditMetadata.btnHelpClick(Sender: TObject);
begin
    OpenHelpUrl(btnHelp.HelpKeyword);
end;

procedure TfrmEditMetadata.btnOKClick(Sender: TObject);
begin
  if frameMetaDataEd.Changed then
  begin
    frameMetaDataEd.SaveXml(FXmlFileName);
    frmModelArchiver.edMetadata.Text := FXmlFileName;
  end;
  Close;
end;

procedure TfrmEditMetadata.btnSearchClick(Sender: TObject);
begin
  if frmSearch = nil then
  begin
    Application.CreateForm(TfrmSearch, frmSearch);
  end;
  frmSearch.Show;
end;

procedure TfrmEditMetadata.EditFile(const XmlFileName: string);
begin
  FXmlFileName := XmlFileName;
  frameMetaDataEd.Enabled := False;
  tmrLoadFile.Enabled := True;
end;

procedure TfrmEditMetadata.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if frmSearch <> nil then
  begin
    frmSearch.Hide;
  end;
end;

procedure TfrmEditMetadata.FormCreate(Sender: TObject);
begin
  btnSearch.Parent := frameMetaDataEd.pnl1;
  btnSearch.Position.Y := frameMetaDataEd.btnDuplicate.Position.Y;
end;

procedure TfrmEditMetadata.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 112 then
  begin
    // F1 key;
    OpenHelpUrl(btnHelp.HelpKeyword);
  end;
end;

procedure TfrmEditMetadata.frameMetaDataEdDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
begin
  if (Length(Data.Files) > 0) and TFile.Exists(Data.Files[0]) then
  begin
    frameMetaDataEd.FrameDragDrop(Sender, Data, Point);
    FXmlFileName := Data.Files[0];
    frameMetaDataEd.Changed := True;
  end;

end;

procedure TfrmEditMetadata.frameMetaDataEdDragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  frameMetaDataEd.FrameDragOver(Sender, Data, Point, Operation);

end;

procedure TfrmEditMetadata.tmrLoadFileTimer(Sender: TObject);
begin
  tmrLoadFile.Enabled := False;
  if TFile.Exists(FXmlFileName) then
  begin
    frameMetaDataEd.OpenFile(FXmlFileName);
  end
  else
  begin
    frameMetaDataEd.btnExpandUsedClick(nil);
  end;
  frameMetaDataEd.Enabled := True;
end;

end.
