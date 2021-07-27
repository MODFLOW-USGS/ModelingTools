unit frmImportSutraFeaturesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Mask, JvExMask, JvToolEdit, Vcl.CheckLst;

type
  TfrmImportSutraFeatures = class(TfrmCustomGoPhast)
    lblModelFeatureFile: TLabel;
    fedModelFeatureFile: TJvFilenameEdit;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    clbFeatures: TCheckListBox;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportSutraFeatures: TfrmImportSutraFeatures;

implementation

uses
  PestFeatureDisplayerUnit, frmGoPhastUnit;

{$R *.dfm}

procedure TfrmImportSutraFeatures.btnOKClick(Sender: TObject);
var
  FileName: TFileName;
  Features: TSutraFeatureTypes;
  Index: TSutraFeatureType;
  FeatureDisplayer: TPestSutraFeatureDisplayer;
begin
  inherited;
  FileName := fedModelFeatureFile.FileName;
  Features := [];
  for Index := Low(TSutraFeatureType) to High(TSutraFeatureType) do
  begin
    if clbFeatures.Checked[Ord(Index)] then
    begin
      Include(Features, Index);
    end;
  end;
  if FileExists(FileName) and (Features <> []) then
  begin
    FeatureDisplayer := TPestSutraFeatureDisplayer.Create(frmGoPhast.PhastModel);
    try
      FeatureDisplayer.ImportFeatures(FileName, Features);
    finally
      FeatureDisplayer.Free;
    end;
  end;
end;

end.
