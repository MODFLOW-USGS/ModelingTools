{#BACKUP MetaData.ico}

// https://www.fgdc.gov/csdgmgraphical/index.html
unit frmMetaDataUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.TreeView, FMX.Styles, FMX.StdCtrls, MetaDataTreeViewItems,
  FMX.Controls.Presentation, MetaDataInterfacesUnit, System.ImageList,
  FMX.ImgList, FMX.Menus, Xml.VerySimple, FMX.Platform, FMX.Edit,
  Mobile.OpenURL, frameMetaDataEditorUnit;

type
  TfrmMetaData = class(TForm)
    dlgSaveMetadata: TSaveDialog;
    MenuBar1: TMenuBar;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    dlgOpenMetaData: TOpenDialog;
    miValidation: TMenuItem;
    miValidationService: TMenuItem;
    frameMetaDataEditor: TframeMetaDataEditor;
    miSearch: TMenuItem;
    miAbout: TMenuItem;
    procedure btnSaveXmlClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure miValidationServiceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miSearchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure frameMetaDataEditorDragDrop(Sender: TObject;
      const Data: TDragObject; const Point: TPointF);
    procedure miAboutClick(Sender: TObject);
  private
    FFileName: string;
    FCaptionRoot: string;
    procedure SetFormCaption(FileName: string);
  public
    { Public declarations }
  end;

var
  frmMetaData: TfrmMetaData;

const
  KFgdcMetaEditorVersion = '1.0';

implementation

uses
  System.Generics.Collections, System.IOUtils, frmSearchUnit,
  frmAboutFgdcMetaEditor;

resourcestring
  StrFgdcMetaEditorS = '%0:s; %1:s';

{$R *.fmx}



procedure TfrmMetaData.btnSaveXmlClick(Sender: TObject);
begin
  if dlgSaveMetadata.FileName <> '' then
  begin
    SetCurrentDir(ExtractFilePath(dlgSaveMetadata.FileName));
  end;
  if dlgSaveMetadata.Execute then
  begin
    frameMetaDataEditor.SaveXml(dlgSaveMetadata.FileName);
    Caption := dlgSaveMetadata.FileName + ': FgdcMetaEditor';
  end;
end;

procedure TfrmMetaData.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if frameMetaDataEditor.Changed then
  begin
    case MessageDlg('Do you want to save the changes to your metadata file?',
        TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo,
        TMsgDlgBtn.mbCancel], 0) of
      mrYes:
        begin
          btnSaveXmlClick(nil);
          if frameMetaDataEditor.Changed then
          begin
            CanClose := False;
          end;
        end;
      mrNo:
        begin
          CanClose := True;
        end
     else
        begin
          CanClose := False;
        end;
    end;
  end;
end;

procedure TfrmMetaData.FormCreate(Sender: TObject);
var
  FileName: string;
begin
  FCaptionRoot := 'FgdcMetaEditor ' + KFgdcMetaEditorVersion;
  Caption := FCaptionRoot;
  if ParamCount >= 1 then
  begin
    FileName := ParamStr(1);
    if TFile.Exists(FileName) then
    begin
      dlgSaveMetadata.FileName := FileName;
      dlgOpenMetaData.FileName := FileName;
      FFileName := FileName;
//      frameMetaDataEditor.OpenFile(FileName);
    end;
  end;
end;

procedure TfrmMetaData.FormShow(Sender: TObject);
var
  FileName: string;
begin
  if FFileName <> '' then
  begin
    FileName := FFileName;
    FFileName := '';
    frameMetaDataEditor.OpenFile(FileName);
    SetFormCaption(FileName);
  end;

end;

procedure TfrmMetaData.frameMetaDataEditorDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
begin
  if (Length(Data.Files) > 0) and TFile.Exists(Data.Files[0]) then
  begin
    frameMetaDataEditor.FrameDragDrop(Sender, Data, Point);
    dlgSaveMetadata.FileName := Data.Files[0];
    dlgOpenMetaData.FileName := dlgSaveMetadata.FileName;
  end;

end;

procedure TfrmMetaData.miAboutClick(Sender: TObject);
begin
  frmAboutMetaEditor.ShowModal
end;

procedure TfrmMetaData.SetFormCaption(FileName: string);
begin
  Caption := Format(StrFgdcMetaEditorS, [FileName, FCaptionRoot]);
end;

procedure TfrmMetaData.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMetaData.miOpenClick(Sender: TObject);
begin
  if dlgOpenMetaData.Execute then
  begin
    dlgSaveMetadata.FileName := dlgOpenMetaData.FileName;
    frameMetaDataEditor.OpenFile(dlgSaveMetadata.FileName);
    SetFormCaption(dlgOpenMetaData.FileName);
//    Caption := Format(StrFgdcMetaEditorS, [dlgOpenMetaData.FileName]);
  end;
end;

procedure TfrmMetaData.miSearchClick(Sender: TObject);
begin
  if frmSearch = nil then
  begin
    Application.CreateForm(TfrmSearch, frmSearch);
  end;
  frmSearch.Show;
end;

procedure TfrmMetaData.miValidationServiceClick(Sender: TObject);
var
  CS: IFMXCursorService;
  ACursor: Integer;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
  begin
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
  end;
  ACursor := 0;
  if Assigned(CS) then
  begin
    ACursor := CS.GetCursor;
    CS.SetCursor(crHourGlass);
  end;
  try
    OpenURL('https://mrdata.usgs.gov/validation/');
  finally
    if Assigned(CS) then
    begin
      CS.SetCursor(ACursor);
    end;
  end;
end;

end.
