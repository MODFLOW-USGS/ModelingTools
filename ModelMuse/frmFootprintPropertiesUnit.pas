unit frmFootprintPropertiesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, ArgusDataEntry, Vcl.Mask, JvExMask, JvSpin,
  UndoItems, FootprintPropertiesUnit;

type
  TUndoChangeFootprintProperties = class(TCustomUndo)
  private
    FNewFootprintProperties: TFootprintProperties;
    FOldFootprintProperties: TFootprintProperties;
  protected
    function Description: string; override;
  public
    constructor Create(var NewFootprintProperties: TFootprintProperties);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmFootprintProperties = class(TfrmCustomGoPhast)
    pnl1: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlMain: TPanel;
    rdeClosureCriterion: TRbwDataEntry;
    lblClosureCriterion: TLabel;
    seMaxIteration: TJvSpinEdit;
    lblMaxIteration: TLabel;
    cbIntitialDistribution: TCheckBox;
    lblRedistribution: TLabel;
    seRedistribution: TJvSpinEdit;
    cbSaveBinary: TCheckBox;
    cbSaveText: TCheckBox;
    cbOpenListFile: TCheckBox;
    lblMinDepthRateIndex: TLabel;
    rdeMinDepthRateIndex: TRbwDataEntry;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure rdeMinDepthRateIndexChange(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFootprintProperties: TfrmFootprintProperties;

implementation

uses
  frmGoPhastUnit;

resourcestring
  StrChangeFootprintPro = 'change WellFootprint properties';

{$R *.dfm}

procedure TfrmFootprintProperties.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmFootprintProperties.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmFootprintProperties.GetData;
var
  Footprint: TFootprintProperties;
begin
  Footprint := frmGoPhast.PhastModel.FootprintProperties;
  rdeClosureCriterion.RealValue := Footprint.ClosureCriterion;
  rdeMinDepthRateIndex.RealValue := Footprint.MinimumDepthRateIndex;
  seMaxIteration.AsInteger := Footprint.MaxIterations;
  cbIntitialDistribution.Checked := Footprint.PerformInitialRedistribution;
  seRedistribution.AsInteger := Footprint.RedistributionCriterion;
  cbSaveBinary.Checked := Footprint.SaveResultsBinary;
  cbSaveText.Checked := Footprint.SaveResultsText;
  cbOpenListFile.Checked := Footprint.OpenInTextEditor;
end;

procedure TfrmFootprintProperties.rdeMinDepthRateIndexChange(Sender: TObject);
begin
  inherited;
  rdeMinDepthRateIndex.Color := clWindow;
  if rdeMinDepthRateIndex.RealValue = 0 then
  begin
    rdeMinDepthRateIndex.Color := clRed
  end;
end;

procedure TfrmFootprintProperties.SetData;
var
  Footprint: TFootprintProperties;
begin
  Footprint := TFootprintProperties.Create(nil);
  try
    Footprint.MaxIterations := seMaxIteration.AsInteger;
    Footprint.ClosureCriterion := rdeClosureCriterion.RealValue;
    Footprint.MinimumDepthRateIndex := rdeMinDepthRateIndex.RealValue;
    Footprint.PerformInitialRedistribution := cbIntitialDistribution.Checked;
    Footprint.RedistributionCriterion := seRedistribution.AsInteger;
    Footprint.SaveResultsBinary := cbSaveBinary.Checked;
    Footprint.SaveResultsText := cbSaveText.Checked;
    Footprint.OpenInTextEditor := cbOpenListFile.Checked;
    frmGoPhast.UndoStack.Submit(TUndoChangeFootprintProperties.Create(Footprint));
  finally
    Footprint.Free
  end;

end;

{ TUndoChangeFootprintProperties }

constructor TUndoChangeFootprintProperties.Create(
  var NewFootprintProperties: TFootprintProperties);
begin
  FNewFootprintProperties := NewFootprintProperties;
  NewFootprintProperties := nil;
  FOldFootprintProperties := TFootprintProperties.Create(nil);
  FOldFootprintProperties.Assign(frmGoPhast.PhastModel.FootprintProperties);
end;

destructor TUndoChangeFootprintProperties.Destroy;
begin
  FNewFootprintProperties.Free;
  FOldFootprintProperties.Free;
  inherited;
end;

procedure TUndoChangeFootprintProperties.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.FootprintProperties := FNewFootprintProperties;
end;

function TUndoChangeFootprintProperties.Description: string;
begin
  result := StrChangeFootprintPro;
end;

procedure TUndoChangeFootprintProperties.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.FootprintProperties := FOldFootprintProperties;
end;

end.
