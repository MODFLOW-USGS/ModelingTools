unit framePackageFmp4SalinityFlushUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Grids, RbwDataGrid4,
  ModflowPackageSelectionUnit, Vcl.Mask, JvExMask, JvSpin, ArgusDataEntry;

type
  TSalinityFlushColumns = (sfcName, sfcTransient, sfcArray);
  TSalinityFlushRows = (sfrName, sfrFarmSaltConcentrations,
    sfrFarmIrrigationUniformity, sfrCropSalinityDemand, sfrCropSalinityTolerance,
    sfrCropMaxLeach, sfrCropLeachRequirement, sfrCropExtraWaterChoice);

  TframePackageFmp4SalinityFlush = class(TframePackage)
    cpnlgrp1: TCategoryPanelGroup;
    cpnlPrint: TCategoryPanel;
    clbPrint: TCheckListBox;
    cpnlOptions: TCategoryPanel;
    pnl1: TPanel;
    rdgSalinityFlush: TRbwDataGrid4;
    seExpressionLength: TJvSpinEdit;
    lblExpressionLength: TLabel;
    rdeExpressionMin: TRbwDataEntry;
    lblExpressionMin: TLabel;
    procedure rdgSalinityFlushSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    procedure InitilizeGrid;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmp4SalinityFlush: TframePackageFmp4SalinityFlush;

implementation

{$R *.dfm}

{ TframePackageFmp4SalinityFlush }

procedure TframePackageFmp4SalinityFlush.GetData(
  Package: TModflowPackageSelection);
  procedure GetFarmOptionGrid(Row: TSalinityFlushRows; Option: TFarmOption);
  begin
    rdgSalinityFlush.Cells[Ord(sfcTransient), Ord(Row)] :=
      DontUseStaticTransient[Ord(Option)];
  end;
  procedure GetArrayListGrid(Row: TSalinityFlushRows; ArrayList: TArrayList);
  begin
    rdgSalinityFlush.ItemIndex[Ord(sfcArray), Ord(Row)] := Ord(ArrayList);
  end;
var
  SalinityFlush: TFarmSalinityFlush;
  PrintIndex: TSalinityFlushPrint;
begin
  cpnlPrint.Collapse;
  if cpnlgrp1.VertScrollBar.Visible then
  begin
    cpnlgrp1.VertScrollBar.Position := 0;
  end;
  inherited;
  SalinityFlush := Package as TFarmSalinityFlush;

  for PrintIndex := Low(TSalinityFlushPrint) to High(TSalinityFlushPrint) do
  begin
    clbPrint.Checked[Ord(PrintIndex)] := PrintIndex in SalinityFlush.SalinityFlushPrints;
  end;

  seExpressionLength.AsInteger := SalinityFlush.ExpressionLength;
  rdeExpressionMin.RealValue := SalinityFlush.ExpressionMin;

  GetFarmOptionGrid(sfrFarmSaltConcentrations, SalinityFlush.FarmSaltConcentrationsChoice);
  GetFarmOptionGrid(sfrFarmIrrigationUniformity, SalinityFlush.FarmIrrigationUniformityChoice);
  GetFarmOptionGrid(sfrCropSalinityDemand, SalinityFlush.CropSalinityDemandChoice);
  GetArrayListGrid(sfrCropSalinityDemand, SalinityFlush.CropSalinityDemandArrayList);
  GetFarmOptionGrid(sfrCropSalinityTolerance, SalinityFlush.CropSalinityToleranceChoice);
  GetFarmOptionGrid(sfrCropMaxLeach, SalinityFlush.CropMaxLeachChoice);
  GetFarmOptionGrid(sfrCropLeachRequirement, SalinityFlush.CropLeachRequirementChoice);
  GetFarmOptionGrid(sfrCropExtraWaterChoice, SalinityFlush.CropExtraWaterChoice);
//  TSalinityFlushRows = (sfrName, sfrFarmSaltConcentrations,
//    sfrFarmIrrigationUniformity, sfrCropSalinityDemand, sfrCropSalinityTolerance,
//    sfrCropMaxLeach, sfrCropLeachRequirement, sfrCropExtraWaterChoice);

end;

procedure TframePackageFmp4SalinityFlush.InitilizeGrid;
begin
  rdgSalinityFlush.BeginUpdate;
  try
    rdgSalinityFlush.FixedCols := 1;

    rdgSalinityFlush.Cells[Ord(sfcTransient), Ord(sfrName)] := 'Frequency';
    rdgSalinityFlush.Cells[Ord(sfcArray), Ord(sfrName)] := 'Array or list';

    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrFarmSaltConcentrations)] := 'Farm salt concentration';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrFarmIrrigationUniformity)] := 'Farm irrigation uniformity';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropSalinityDemand)] := 'Crop salinity demand';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropSalinityTolerance)] := 'Crop salinity tolerance';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropMaxLeach)] := 'Crop max leach';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropLeachRequirement)] := 'Crop Leach requirement';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropExtraWaterChoice)] := 'Crop extra water';
  finally
  rdgSalinityFlush.EndUpdate;
  end;
end;

procedure TframePackageFmp4SalinityFlush.Loaded;
begin
  inherited;
  InitilizeGrid;
end;

procedure TframePackageFmp4SalinityFlush.rdgSalinityFlushSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  SfRow: TSalinityFlushRows;
begin
  inherited;
  if (ACol = Ord(sfcArray)) then
  begin
    SfRow := TSalinityFlushRows(ARow);
    CanSelect := SfRow = sfrCropSalinityDemand;
  end;

end;

procedure TframePackageFmp4SalinityFlush.SetData(
  Package: TModflowPackageSelection);
  function SetFarmOptionGrid(Row: TSalinityFlushRows): TFarmOption;
  begin
    Result := TFarmOption(DontUseStaticTransient.IndexOf(rdgSalinityFlush.Cells[Ord(sfcTransient), Ord(Row)]));
  end;
  function SetArrayListGrid(Row: TSalinityFlushRows): TArrayList;
  begin
    result := TArrayList(rdgSalinityFlush.ItemIndex[Ord(sfcArray), Ord(Row)]);
  end;
var
  SalinityFlush: TFarmSalinityFlush;
  PrintOptions: TSalinityFlushPrints;
  PrintIndex: TSalinityFlushPrint;
begin
  inherited;
  SalinityFlush := Package as TFarmSalinityFlush;

  PrintOptions := [];
  for PrintIndex := Low(TSalinityFlushPrint) to High(TSalinityFlushPrint) do
  begin
    if clbPrint.Checked[Ord(PrintIndex)] then
    begin
      Include(PrintOptions, PrintIndex);
    end;
  end;
  SalinityFlush.SalinityFlushPrints := PrintOptions;

  seExpressionLength.AsInteger := SalinityFlush.ExpressionLength;
  rdeExpressionMin.RealValue := SalinityFlush.ExpressionMin;

  SalinityFlush.FarmSaltConcentrationsChoice := SetFarmOptionGrid(sfrFarmSaltConcentrations);
  SalinityFlush.FarmIrrigationUniformityChoice := SetFarmOptionGrid(sfrFarmIrrigationUniformity);
  SalinityFlush.CropSalinityDemandChoice := SetFarmOptionGrid(sfrCropSalinityDemand);
  SalinityFlush.CropSalinityDemandArrayList := SetArrayListGrid(sfrCropSalinityDemand);
  SalinityFlush.CropSalinityToleranceChoice := SetFarmOptionGrid(sfrCropSalinityTolerance);
  SalinityFlush.CropMaxLeachChoice := SetFarmOptionGrid(sfrCropMaxLeach);
  SalinityFlush.CropLeachRequirementChoice := SetFarmOptionGrid(sfrCropLeachRequirement);
  SalinityFlush.CropExtraWaterChoice := SetFarmOptionGrid(sfrCropExtraWaterChoice);
end;

end.
