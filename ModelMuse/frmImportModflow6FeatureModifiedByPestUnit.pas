unit frmImportModflow6FeatureModifiedByPestUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Mask, JvExMask, JvToolEdit, JvSpin, Vcl.Buttons, Vcl.ExtCtrls;

type
  TfrmImportModflow6FeatureModifiedByPest = class(TfrmCustomGoPhast)
    fedModelFeatureFile: TJvFilenameEdit;
    lblModelFeatureFile: TLabel;
    seStressPeriod: TJvSpinEdit;
    lblStressPeriod: TLabel;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    procedure btnOKClick(Sender: TObject);
  private
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportModflow6FeatureModifiedByPest: TfrmImportModflow6FeatureModifiedByPest;

implementation

uses PestFeatureDisplayerUnit, frmGoPhastUnit, Modflow6Importer;

{$R *.dfm}

procedure TfrmImportModflow6FeatureModifiedByPest.btnOKClick(Sender: TObject);
begin
  inherited;
  if FileExists(fedModelFeatureFile.FileName) then
  begin
    SetData;
  end;
end;

procedure TfrmImportModflow6FeatureModifiedByPest.SetData;
var
  Importer: TPestModflow6FeatureDisplayer;
  GridType: TModflow6GridType;
begin
  Importer := TPestModflow6FeatureDisplayer.Create(frmGoPhast.PhastModel);
  try
    if frmGoPhast.PhastModel.DisvUsed then
    begin
      GridType := mggrDisv;
    end
    else
    begin
      GridType := m6gtStructured;
    end;
    Importer.ImportFeatures(fedModelFeatureFile.FileName, GridType,
      seStressPeriod.AsInteger);
  finally
    Importer.Free;
  end;
end;

end.
