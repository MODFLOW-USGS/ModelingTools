unit framePackageFmp4LandUseUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4,
  ModflowPackageSelectionUnit, ArgusDataEntry;

type
  TSoilOptionColumns = (socName, socTransient, socArray, socOther);
  TSoilOptionRows = (sorName, sorSoilLocation, sorLandUseFraction,
    sorCropCoeffOrUse, sorIrrigation, sorRootDepth, sorRootPressure,
    sorTranspirationFraction, sorEvapIrrigationFraction,
    sorFractionOfPrecipToSurfaceWater, sorFractionOfIrrigationToSurfaceWater,
    sorPondDepth, sorAddedDemand, sorNoCropUseMeansBareSoil,
    sorET_IrrigFracCorrection);

  TframePackageFmp4LandUse = class(TframePackage)
    cpnlgrp1: TCategoryPanelGroup;
    cpnlPrint: TCategoryPanel;
    clbPrint: TCheckListBox;
    cpnlOptions: TCategoryPanel;
    rdgSoils: TRbwDataGrid4;
    pnl1: TPanel;
    comboLandUsePerCell: TComboBox;
    lblLandUsePerCell: TLabel;
    rdeMinimumBareFraction: TRbwDataEntry;
    lblMinimumBareFraction: TLabel;
    rdeRelaxFracHeadChange: TRbwDataEntry;
    lblRelaxFracHeadChange: TLabel;
    pnl2: TPanel;
    comboSpecifyCrops: TComboBox;
    lblSpecifyCrops: TLabel;
    procedure rdgSoilsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    procedure InitializeGrid;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmp4LandUse: TframePackageFmp4LandUse;

implementation

{$R *.dfm}

{ TframePackageFmp4LandUse }

procedure TframePackageFmp4LandUse.GetData(Package: TModflowPackageSelection);
var
  LandUsePackage: TFarmLandUse;
  PrintIndex: TLandUsePrint;
  procedure GetFarmOptionGrid(Row: TSoilOptionRows; Option: TFarmOption);
  begin
    rdgSoils.Cells[Ord(socTransient), Ord(Row)] :=
      DontUseStaticTransient[Ord(Option)];
  end;
  procedure GetArrayListGrid(Row: TSoilOptionRows; ArrayList: TArrayList);
  begin
    rdgSoils.ItemIndex[Ord(socArray), Ord(Row)] := Ord(ArrayList);
  end;
begin
  cpnlPrint.Collapse;
  inherited;
  LandUsePackage := Package as TFarmLandUse;

  for PrintIndex := Low(TLandUsePrint) to High(TLandUsePrint) do
  begin
    clbPrint.Checked[Ord(PrintIndex)] := PrintIndex in LandUsePackage.LandUsePrints;
  end;

  comboLandUsePerCell.ItemIndex := Ord(LandUsePackage.LandUseOption);
  rdeMinimumBareFraction.RealValue := LandUsePackage.MinimumBareFraction;
  rdeRelaxFracHeadChange.RealValue := LandUsePackage.RelaxFracHeadChange;

  rdgSoils.Cells[Ord(socTransient), Ord(sorSoilLocation)] := StaticTransient[Ord(LandUsePackage.SoilLocationChoice)];

  GetFarmOptionGrid(sorLandUseFraction, LandUsePackage.LandUseFractionChoice);
  GetArrayListGrid(sorLandUseFraction, LandUsePackage.LandUseFractionArrayList);

  GetFarmOption(comboSpecifyCrops, LandUsePackage.SpecifyCropsToPrint);

  GetFarmOptionGrid(sorCropCoeffOrUse, LandUsePackage.CropCoeffOrUseChoice);
  GetArrayListGrid(sorCropCoeffOrUse, LandUsePackage.CropCoefOrUseArrayList);
  rdgSoils.ItemIndex[Ord(socOther), Ord(sorCropCoeffOrUse)] := Ord(LandUsePackage.CropCoefOrUse);

  GetFarmOptionGrid(sorIrrigation, LandUsePackage.IrrigationChoice);
  GetArrayListGrid(sorIrrigation, LandUsePackage.IrrigationArrayList);

  GetFarmOptionGrid(sorRootDepth, LandUsePackage.RootDepthChoice);
  GetArrayListGrid(sorRootDepth, LandUsePackage.RootDepthArrayList);

  GetFarmOptionGrid(sorRootPressure, LandUsePackage.RootPressureChoice);

  GetFarmOptionGrid(sorTranspirationFraction, LandUsePackage.TranspirationFractionChoice);
  GetArrayListGrid(sorTranspirationFraction, LandUsePackage.TranspirationFractionArrayList);

  GetFarmOptionGrid(sorEvapIrrigationFraction, LandUsePackage.EvapIrrigationFractionChoice);
  GetArrayListGrid(sorEvapIrrigationFraction, LandUsePackage.EvapIrrigationFractionArrayList);

  GetFarmOptionGrid(sorFractionOfPrecipToSurfaceWater, LandUsePackage.FractionOfPrecipToSurfaceWater);
  GetArrayListGrid(sorFractionOfPrecipToSurfaceWater, LandUsePackage.FractionOfPrecipToSurfaceWaterArrayList);

  GetFarmOptionGrid(sorFractionOfIrrigationToSurfaceWater, LandUsePackage.FractionOfIrrigationToSurfaceWater);
  GetArrayListGrid(sorFractionOfIrrigationToSurfaceWater, LandUsePackage.FractionOfIrrigationToSurfaceWaterArrayList);

  GetFarmOptionGrid(sorPondDepth, LandUsePackage.PondDepthChoice);

  GetFarmOptionGrid(sorAddedDemand, LandUsePackage.AddedDemandChoice);
  GetArrayListGrid(sorAddedDemand, LandUsePackage.AddedDemandArrayList);

  GetFarmOptionGrid(sorNoCropUseMeansBareSoil, LandUsePackage.NoCropUseMeansBareSoilChoice);

  GetFarmOptionGrid(sorET_IrrigFracCorrection, LandUsePackage.ET_IrrigFracCorrectionChoice);

end;

procedure TframePackageFmp4LandUse.InitializeGrid;
begin
  rdgSoils.BeginUpdate;
  try
    rdgSoils.Cells[Ord(socTransient), Ord(sorName)] := 'Frequency';
    rdgSoils.Cells[Ord(socArray), Ord(sorName)] := 'Array or list';
    rdgSoils.Cells[Ord(socOther), Ord(sorName)] := 'Method';

    rdgSoils.Cells[Ord(socName), Ord(sorSoilLocation)] := 'Soil location';
    rdgSoils.Cells[Ord(socName), Ord(sorLandUseFraction)] := 'Land use fraction';
    rdgSoils.Cells[Ord(socName), Ord(sorCropCoeffOrUse)] := 'Crop coeff. or use';
    rdgSoils.Cells[Ord(socName), Ord(sorIrrigation)] := 'Irrigation';
    rdgSoils.Cells[Ord(socName), Ord(sorRootDepth)] := 'Root depth';
    rdgSoils.Cells[Ord(socName), Ord(sorRootPressure)] := 'Root pressure';
    rdgSoils.Cells[Ord(socName), Ord(sorTranspirationFraction)] := 'Transpiration fraction';
    rdgSoils.Cells[Ord(socName), Ord(sorEvapIrrigationFraction)] := 'Evap irrig. fraction';
    rdgSoils.Cells[Ord(socName), Ord(sorFractionOfPrecipToSurfaceWater)] := 'Fraction of precip. to surface water';
    rdgSoils.Cells[Ord(socName), Ord(sorFractionOfIrrigationToSurfaceWater)] := 'Fraction of irrig. to surface water';
    rdgSoils.Cells[Ord(socName), Ord(sorPondDepth)] := 'Pond depth';
    rdgSoils.Cells[Ord(socName), Ord(sorAddedDemand)] := 'Added demand';
    rdgSoils.Cells[Ord(socName), Ord(sorNoCropUseMeansBareSoil)] := 'No crop means bare soil';
    rdgSoils.Cells[Ord(socName), Ord(sorET_IrrigFracCorrection)] := 'ET irrig. frac. correction';
  finally
    rdgSoils.EndUpdate;
  end;
end;

{
  TSoilOptionColumns = (socName, socTransient, socArray, socOther);

  TSoilOptionRows = (sorName, sorSoilLocation, sorLandUseFraction,
    sorCropCoeffOrUse, sorIrrigation, sorRootDepth, sorRootPressure,
    sorTranspirationFraction, sorEvapIrrigationFraction,
    sorFractionOfPrecipToSurfaceWater, sorFractionOfIrrigationToSurfaceWater,
    sorPondDepth, sorAddedDemand, sorNoCropUseMeansBareSoil,
    sorET_IrrigFracCorrection);
}

procedure TframePackageFmp4LandUse.Loaded;
begin
  inherited;
  InitializeGrid;
end;

procedure TframePackageFmp4LandUse.rdgSoilsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Column: TRbwColumn4;
  SoilRow: TSoilOptionRows;
begin
  inherited;
  if (ACol = Ord(socTransient)) and not rdgSoils.Drawing then
  begin
    Column := rdgSoils.Columns[Ord(socTransient)];
    if (ARow = Ord(sorSoilLocation)) then
    begin
      Column.PickList := StaticTransient;
    end
    else
    begin
      Column.PickList := DontUseStaticTransient;
    end;
  end;

  if (ACol = Ord(socArray)) then
  begin
    SoilRow := TSoilOptionRows(ARow);
    CanSelect := SoilRow in [sorLandUseFraction, sorCropCoeffOrUse,
      sorIrrigation, sorRootDepth, sorTranspirationFraction,
      sorEvapIrrigationFraction, sorFractionOfPrecipToSurfaceWater,
      sorFractionOfIrrigationToSurfaceWater, sorAddedDemand];
  end;

  if (ACol = Ord(socOther)) then
  begin
    SoilRow := TSoilOptionRows(ARow);
    CanSelect := SoilRow = sorCropCoeffOrUse;
  end;

end;

procedure TframePackageFmp4LandUse.SetData(Package: TModflowPackageSelection);
  function SetFarmOptionGrid(Row: TSoilOptionRows): TFarmOption;
  begin
    Result := TFarmOption(DontUseStaticTransient.IndexOf(rdgSoils.Cells[Ord(socTransient), Ord(Row)]));
  end;
  function SetArrayListGrid(Row: TSoilOptionRows): TArrayList;
  begin
    result := TArrayList(rdgSoils.ItemIndex[Ord(socArray), Ord(Row)]);
  end;
var
  LandUsePackage: TFarmLandUse;
  PrintIndex: TLandUsePrint;
  PrintChoices: TLandUsePrints;
begin
  inherited;

  LandUsePackage := Package as TFarmLandUse;

  PrintChoices := [];
  for PrintIndex := Low(TLandUsePrint) to High(TLandUsePrint) do
  begin
    if clbPrint.Checked[Ord(PrintIndex)] then
    begin
      Include(PrintChoices, PrintIndex)
    end;
  end;
  LandUsePackage.LandUsePrints := PrintChoices;

  LandUsePackage.LandUseOption := TLandUseOption(comboLandUsePerCell.ItemIndex);
  LandUsePackage.MinimumBareFraction := rdeMinimumBareFraction.RealValue;
  LandUsePackage.RelaxFracHeadChange := rdeRelaxFracHeadChange.RealValue;

  LandUsePackage.SoilLocationChoice := TRequiredSteadyTransient(
    StaticTransient.IndexOf(rdgSoils.Cells[Ord(socTransient), Ord(sorSoilLocation)]));

  LandUsePackage.LandUseFractionChoice := SetFarmOptionGrid(sorLandUseFraction);
  LandUsePackage.LandUseFractionArrayList := SetArrayListGrid(sorLandUseFraction);

  LandUsePackage.SpecifyCropsToPrint := SetFarmOption(comboSpecifyCrops);

  LandUsePackage.CropCoeffOrUseChoice := SetFarmOptionGrid(sorCropCoeffOrUse);
  LandUsePackage.CropCoefOrUseArrayList := SetArrayListGrid(sorCropCoeffOrUse);
  LandUsePackage.CropCoefOrUse :=
    TCropCoefOrUse(rdgSoils.ItemIndex[Ord(socOther), Ord(sorCropCoeffOrUse)]);

  LandUsePackage.IrrigationChoice := SetFarmOptionGrid(sorIrrigation);
  LandUsePackage.IrrigationArrayList := SetArrayListGrid(sorIrrigation);

  LandUsePackage.RootDepthChoice := SetFarmOptionGrid(sorRootDepth);
  LandUsePackage.RootDepthArrayList := SetArrayListGrid(sorRootDepth);

  LandUsePackage.RootPressureChoice := SetFarmOptionGrid(sorRootPressure);

  LandUsePackage.TranspirationFractionChoice := SetFarmOptionGrid(sorTranspirationFraction);
  LandUsePackage.TranspirationFractionArrayList := SetArrayListGrid(sorTranspirationFraction);

  LandUsePackage.EvapIrrigationFractionChoice := SetFarmOptionGrid(sorEvapIrrigationFraction);
  LandUsePackage.EvapIrrigationFractionArrayList := SetArrayListGrid(sorEvapIrrigationFraction);

  LandUsePackage.FractionOfPrecipToSurfaceWater := SetFarmOptionGrid(sorFractionOfPrecipToSurfaceWater);
  LandUsePackage.FractionOfPrecipToSurfaceWaterArrayList := SetArrayListGrid(sorFractionOfPrecipToSurfaceWater);

  LandUsePackage.FractionOfIrrigationToSurfaceWater := SetFarmOptionGrid(sorFractionOfIrrigationToSurfaceWater);
  LandUsePackage.FractionOfIrrigationToSurfaceWaterArrayList := SetArrayListGrid(sorFractionOfIrrigationToSurfaceWater);

  LandUsePackage.PondDepthChoice := SetFarmOptionGrid(sorPondDepth);

  LandUsePackage.AddedDemandChoice := SetFarmOptionGrid(sorAddedDemand);
  LandUsePackage.AddedDemandArrayList := SetArrayListGrid(sorAddedDemand);

  LandUsePackage.NoCropUseMeansBareSoilChoice := SetFarmOptionGrid(sorNoCropUseMeansBareSoil);

  LandUsePackage.ET_IrrigFracCorrectionChoice := SetFarmOptionGrid(sorET_IrrigFracCorrection);

end;

//initialization
//  CropCoOrUseChoices := TStringList.Create;
//  CropCoOrUseChoices.Add('Crop Coefficient');
//  CropCoOrUseChoices.Add('Consumptive Use');
//
//finalization
//  CropCoOrUseChoices.Free;

end.
