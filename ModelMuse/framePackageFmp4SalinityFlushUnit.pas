unit framePackageFmp4SalinityFlushUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Grids, RbwDataGrid4,
  ModflowPackageSelectionUnit, Vcl.Mask, JvExMask, JvSpin, ArgusDataEntry;

type
  TSalinityFlushColumns = (sfcName, sfcTransient, sfcArray, sfcSFAC, sfcFile, sfcSfacFile);
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

const
  RequiredRows = [sfrCropSalinityTolerance, sfrCropLeachRequirement, sfrCropExtraWaterChoice];

implementation

uses
  GoPhastTypes;

resourcestring
  StrFarmSaltConcentrat = 'WBS salt concentration';
  StrFarmIrrigationUnif = 'WBS irrigation uniformity';
  StrCropSalinityDemand = 'Crop salinity demand';
  StrCropSalinityTolera = 'Crop salinity tolerance';
  StrCropMaxLeach = 'Crop max leach';
  StrCropLeachRequireme = 'Crop leaching requirement';
  StrCropExtraWater = 'Crop extra water';

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
  procedure GetFarmProperty(FarmProperty: TFarmProperty; ARow: Integer);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcTransient), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.Cells[Ord(sfcTransient), ARow] := DontUseStaticTransient[Ord(FarmProperty.FarmOption)];
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcArray), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.ItemIndex[Ord(sfcArray), ARow] := Ord(FarmProperty.ArrayList);
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcSFAC), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.Cells[Ord(sfcSFAC), ARow] := FarmProperty.UnitConversionScaleFactor;
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.Cells[Ord(sfcFile), ARow] := FarmProperty.ExternalFileName;
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcSfacFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.Cells[Ord(sfcSfacFile), ARow] := FarmProperty.ExternalScaleFileName;
    end;
//  TSalinityFlushColumns = (sfcName, sfcTransient, sfcArray, sfcSFAC, sfcFile, sfcSfacFile);
  end;
var
  SalinityFlush: TFarmProcess4SalinityFlush;
  PrintIndex: TSalinityFlushPrint;
begin
  cpnlPrint.Collapse;
  if cpnlgrp1.VertScrollBar.Visible then
  begin
    cpnlgrp1.VertScrollBar.Position := 0;
  end;
  inherited;
  SalinityFlush := Package as TFarmProcess4SalinityFlush;

  for PrintIndex := Low(TSalinityFlushPrint) to High(TSalinityFlushPrint) do
  begin
    clbPrint.Checked[Ord(PrintIndex)] := PrintIndex in SalinityFlush.SalinityFlushPrints;
  end;

  rdeExpressionMin.RealValue := SalinityFlush.ExpressionMin;

  GetFarmProperty(SalinityFlush.FarmSaltConcentrationsChoice, Ord(sfrFarmSaltConcentrations));
  GetFarmProperty(SalinityFlush.FarmIrrigationUniformityChoice, Ord(sfrFarmIrrigationUniformity));
  GetFarmProperty(SalinityFlush.CropSalinityDemandChoice, Ord(sfrCropSalinityDemand));
  GetFarmProperty(SalinityFlush.CropSalinityToleranceChoice, Ord(sfrCropSalinityTolerance));
  GetFarmProperty(SalinityFlush.CropMaxLeachChoice, Ord(sfrCropMaxLeach));
  GetFarmProperty(SalinityFlush.CropLeachRequirementChoice, Ord(sfrCropLeachRequirement));
  GetFarmProperty(SalinityFlush.CropExtraWaterChoice, Ord(sfrCropExtraWaterChoice));
end;

procedure TframePackageFmp4SalinityFlush.InitilizeGrid;
begin
  rdgSalinityFlush.BeginUpdate;
  try
    rdgSalinityFlush.FixedCols := 1;

    rdgSalinityFlush.Cells[Ord(sfcTransient), Ord(sfrName)] := StrFrequency;
    rdgSalinityFlush.Cells[Ord(sfcArray), Ord(sfrName)] := StrArrayOrList;
    rdgSalinityFlush.Cells[Ord(sfcSFAC), Ord(sfrName)] := StrUnitConversionScal;
    rdgSalinityFlush.Cells[Ord(sfcFile), Ord(sfrName)] := StrExternallyGenerated;
    rdgSalinityFlush.Cells[Ord(sfcSfacFile), Ord(sfrName)] := StrExternallyGeneratedSfac;

    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrFarmSaltConcentrations)] := StrFarmSaltConcentrat;
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrFarmIrrigationUniformity)] := StrFarmIrrigationUnif;
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropSalinityDemand)] := StrCropSalinityDemand;
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropSalinityTolerance)] := StrCropSalinityTolera;
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropMaxLeach)] := StrCropMaxLeach;
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropLeachRequirement)] := StrCropLeachRequireme;
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrCropExtraWaterChoice)] := StrCropExtraWater;
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
  Column: TRbwColumn4;
  ItemIndex: Integer;
  FarmOption: TFarmOption;
begin
  inherited;
  if (ACol = Ord(sfcArray)) then
  begin
    SfRow := TSalinityFlushRows(ARow);
    CanSelect := SfRow = sfrCropSalinityDemand;
  end;
  if (ACol = Ord(sfcTransient)) and not rdgSalinityFlush.Drawing then
  begin
    Column := rdgSalinityFlush.Columns[Ord(sfcTransient)];
    SfRow := TSalinityFlushRows(ARow);
    if SfRow in RequiredRows then
    begin
      Column.PickList := StaticTransient;
    end
    else
    begin
      Column.PickList := DontUseStaticTransient;
    end;
  end;
  if ACol in [Ord(sfcSFAC), Ord(sfcFile), Ord(sfcSfacFile)] then
  begin
    SfRow := TSalinityFlushRows(ARow);
    ItemIndex := DontUseStaticTransient.IndexOf(
      rdgSalinityFlush.Cells[Ord(sfcTransient), Ord(ARow)]);
    if ItemIndex < 0 then
    begin
      ItemIndex := 0;
    end;
    if (ItemIndex = 0) and (SfRow in RequiredRows) then
    begin
      ItemIndex := 1;
    end;
    FarmOption := TFarmOption(ItemIndex);
    if FarmOption = foNotUsed then
    begin
      CanSelect := False
    end;
  end;
  if ACol in [Ord(sfcSFAC), Ord(sfcSfacFile)] then
  begin
    SfRow := TSalinityFlushRows(ARow);
    if SfRow = sfrCropSalinityDemand then
    begin
      CanSelect := False
    end;
  end;
{{
  TSalinityFlushColumns = (sfcName, sfcTransient, sfcArray, sfcSFAC, sfcFile, sfcSfacFile);
  TSalinityFlushRows = (sfrName, sfrFarmSaltConcentrations,
    sfrFarmIrrigationUniformity, sfrCropSalinityDemand, sfrCropSalinityTolerance,
    sfrCropMaxLeach, sfrCropLeachRequirement, sfrCropExtraWaterChoice);
}
end;

procedure TframePackageFmp4SalinityFlush.SetData(
  Package: TModflowPackageSelection);
  function SetFarmOptionGrid(Row: TSalinityFlushRows): TFarmOption;
  var
    ItemIndex: Integer;
  begin
    ItemIndex := DontUseStaticTransient.IndexOf(
      rdgSalinityFlush.Cells[Ord(sfcTransient), Ord(Row)]);
    if ItemIndex < 0 then
    begin
      ItemIndex := 0;
    end;
    if (ItemIndex = 0) and (Row in RequiredRows) then
    begin
      ItemIndex := 1;
    end;
    Result := TFarmOption(ItemIndex);
  end;
  function SetArrayListGrid(Row: TSalinityFlushRows): TArrayList;
  begin
    result := TArrayList(rdgSalinityFlush.ItemIndex[Ord(sfcArray), Ord(Row)]);
  end;
  procedure SetFarmProperty(FarmProperty: TFarmProperty; ARow: TSalinityFlushRows);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcTransient), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.FarmOption := SetFarmOptionGrid(ARow);
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcArray), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ArrayList := SetArrayListGrid(ARow);
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcSFAC), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.UnitConversionScaleFactor :=
        rdgSalinityFlush.Cells[Ord(sfcSFAC), Ord(ARow)];
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalFileName :=
        rdgSalinityFlush.Cells[Ord(sfcFile), Ord(ARow)];
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcSfacFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalScaleFileName :=
        rdgSalinityFlush.Cells[Ord(sfcSfacFile), Ord(ARow)];
    end;
  end;
var
  SalinityFlush: TFarmProcess4SalinityFlush;
  PrintOptions: TSalinityFlushPrints;
  PrintIndex: TSalinityFlushPrint;
begin
  inherited;
  SalinityFlush := Package as TFarmProcess4SalinityFlush;

  PrintOptions := [];
  for PrintIndex := Low(TSalinityFlushPrint) to High(TSalinityFlushPrint) do
  begin
    if clbPrint.Checked[Ord(PrintIndex)] then
    begin
      Include(PrintOptions, PrintIndex);
    end;
  end;
  SalinityFlush.SalinityFlushPrints := PrintOptions;

  SalinityFlush.ExpressionMin := rdeExpressionMin.RealValue;

  SetFarmProperty(SalinityFlush.FarmSaltConcentrationsChoice, sfrFarmSaltConcentrations);
  SetFarmProperty(SalinityFlush.FarmIrrigationUniformityChoice, sfrFarmIrrigationUniformity);
  SetFarmProperty(SalinityFlush.CropSalinityDemandChoice, sfrCropSalinityDemand);
  SetFarmProperty(SalinityFlush.CropSalinityToleranceChoice, sfrCropSalinityTolerance);
  SetFarmProperty(SalinityFlush.CropMaxLeachChoice, sfrCropMaxLeach);
  SetFarmProperty(SalinityFlush.CropLeachRequirementChoice, sfrCropLeachRequirement);
  SetFarmProperty(SalinityFlush.CropExtraWaterChoice, sfrCropExtraWaterChoice);
end;

end.
