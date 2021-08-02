unit frmImportSutraFeaturesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Mask, JvExMask, JvToolEdit, Vcl.CheckLst,
  JvSpin, PestFeatureDisplayerUnit;

type
  TfrmImportSutraFeatures = class(TfrmCustomGoPhast)
    lblModelFeatureFile: TLabel;
    fedModelFeatureFile: TJvFilenameEdit;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    clbFeatures: TCheckListBox;
    seTimeStep: TJvSpinEdit;
    lblTimeStep: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure fedModelFeatureFileChange(Sender: TObject);
    procedure clbFeaturesClickCheck(Sender: TObject);
  private
    procedure SetData(const FileName: string; Features: TSutraFeatureTypes);
    procedure EnableOkButton;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportSutraFeatures: TfrmImportSutraFeatures;

implementation

uses
  frmGoPhastUnit, SutraImporter;

{$R *.dfm}

procedure TfrmImportSutraFeatures.clbFeaturesClickCheck(Sender: TObject);
begin
  inherited;
  EnableOkButton;
end;

procedure TfrmImportSutraFeatures.EnableOkButton;
var
  BoundaryTypeIndex: Integer;
  ShouldEnable: Boolean;
begin
  ShouldEnable := FileExists(fedModelFeatureFile.FileName);
  if ShouldEnable then
  begin
    ShouldEnable := False;
    for BoundaryTypeIndex := 0 to clbFeatures.Items.Count - 1 do
    begin
      if clbFeatures.Checked[BoundaryTypeIndex] then
      begin
        ShouldEnable := True;
        break;
      end;
    end;
  end;
  btnOK.Enabled := ShouldEnable;
end;

procedure TfrmImportSutraFeatures.fedModelFeatureFileChange(Sender: TObject);
begin
  inherited;
  EnableOkButton;
  seTimeStep.Enabled :=
    SameText(ExtractFileName(fedModelFeatureFile.FileName), 'sutra.fil');
end;

procedure TfrmImportSutraFeatures.SetData(const FileName: string; Features: TSutraFeatureTypes);
var
  FeatureDisplayer: TPestSutraFeatureDisplayer;
begin
  FeatureDisplayer := TPestSutraFeatureDisplayer.Create(frmGoPhast.PhastModel);
  try
    try
      FeatureDisplayer.ImportFeatures(FileName, Features, seTimeStep.AsInteger);
    except on E: EImportSutraError do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    FeatureDisplayer.Free;
  end;
end;

procedure TfrmImportSutraFeatures.btnOKClick(Sender: TObject);
var
  FileName: TFileName;
  Features: TSutraFeatureTypes;
  Index: TSutraFeatureType;
begin
  inherited;
  FileName := fedModelFeatureFile.FileName;
  Assert(FileExists(FileName));
  Features := [];
  for Index := Low(TSutraFeatureType) to High(TSutraFeatureType) do
  begin
    if clbFeatures.Checked[Ord(Index)] then
    begin
      Include(Features, Index);
    end;
  end;
  Assert(Features <> []);
  SetData(FileName, Features);
end;

end.
