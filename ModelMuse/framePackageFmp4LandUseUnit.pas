unit framePackageFmp4LandUseUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4,
  ModflowPackageSelectionUnit, ArgusDataEntry;

type
  TSoilOptionColumns = (socName, socTransient, socArray, socOther, socSFAC,
    socFile, socSfacFile);

  TSoilOptionRows = (sorName, sorSoilLocation, sorLandUseFraction,
    sorCropCoeff, sorConsumptiveUse, sorIrrigation, sorRootDepth,
    sorRootPressure, sorGroundwaterRootInteraction,
    sorTranspirationFraction, sorEvapIrrigationFraction,
    sorFractionOfPrecipToSurfaceWater, sorFractionOfIrrigationToSurfaceWater,
    sorPondDepth, sorAddedDemand, sorNoCropUseMeansBareSoil,
    sorET_IrrigFracCorrection);

  TframePackageFmp4LandUse = class(TframePackage)
    cpnlgrp1: TCategoryPanelGroup;
    cpnlPrint: TCategoryPanel;
    clbPrint: TCheckListBox;
    cpnlOptions: TCategoryPanel;
    rdgLandUse: TRbwDataGrid4;
    comboLandUsePerCell: TComboBox;
    lblLandUsePerCell: TLabel;
    rdeMinimumBareFraction: TRbwDataEntry;
    lblMinimumBareFraction: TLabel;
    rdeRelaxFracHeadChange: TRbwDataEntry;
    lblRelaxFracHeadChange: TLabel;
    pnl2: TPanel;
    comboSpecifyCrops: TComboBox;
    lblSpecifyCrops: TLabel;
    cpnlDataSets: TCategoryPanel;
    procedure rdgLandUseSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure comboLandUsePerCellChange(Sender: TObject);
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

uses
  GoPhastTypes;

resourcestring
  StrCropLocation = 'Crop location';
  StrLandUseFraction = 'Land use fraction';
  StrCropCoeff = 'Crop coeff.';
  StrConsumptiveUse = 'Consumptive use';
  StrIrrigation = 'Irrigation';
  StrRootDepth = 'Root depth';
  StrRootPressure = 'Root pressure';
  StrTranspirationFracti = 'Transpiration fraction';
  StrEvapIrrigFraction = 'Evap irrig. fraction';
  StrFractionOfPrecip = 'Fraction of precip. to surface water';
  StrFractionOfIrrigT = 'Fraction of irrig. to surface water';
  StrPondDepth = 'Pond depth';
  StrAddedDemand = 'Added demand';
  StrNoCropMeansBareS = 'No crop means bare soil';
  StrETIrrigFracCorr = 'ET irrig. frac. correction';
  StrGroundwaterRootInt = 'Groundwater Root Interaction';
  StrOption = 'Key Word';

var
  ByCropIrrigate: TStringList;
  LengthRate: TStringList;

{$R *.dfm}

{ TframePackageFmp4LandUse }

procedure TframePackageFmp4LandUse.comboLandUsePerCellChange(Sender: TObject);
begin
  inherited;
  if (comboLandUsePerCell.ItemIndex = 1)
    and (rdgLandUse.ItemIndex[Ord(socTransient), Ord(sorLandUseFraction)] = 0) then
  begin
    rdgLandUse.ItemIndex[Ord(socTransient), Ord(sorLandUseFraction)] := 1
  end;
end;

procedure TframePackageFmp4LandUse.GetData(Package: TModflowPackageSelection);
var
  LandUsePackage: TFarmProcess4LandUse;
  PrintIndex: TLandUsePrint;
  procedure GetFarmOptionGrid(Row: TSoilOptionRows; Option: TFarmOption);
  begin
    rdgLandUse.Cells[Ord(socTransient), Ord(Row)] :=
      DontUseStaticTransient[Ord(Option)];
  end;
  procedure GetArrayListGrid(Row: TSoilOptionRows; ArrayList: TArrayList);
  begin
    rdgLandUse.ItemIndex[Ord(socArray), Ord(Row)] := Ord(ArrayList);
  end;
  procedure GetFarmProperty(FarmProperty: TFarmProperty; ARow: Integer);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socTransient), ARow, CanSelect);
    if CanSelect then
    begin
      rdgLandUse.ItemIndex[Ord(socTransient), ARow] := Ord(FarmProperty.FarmOption);
    end;

    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socArray), ARow, CanSelect);
    if CanSelect then
    begin
      rdgLandUse.ItemIndex[Ord(socArray), ARow] := Ord(FarmProperty.ArrayList);
    end;

    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socSFAC), ARow, CanSelect);
    if CanSelect then
    begin
      rdgLandUse.Cells[Ord(socSFAC), ARow] := FarmProperty.UnitConversionScaleFactor;
    end;

    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgLandUse.Cells[Ord(socFile), ARow] := FarmProperty.ExternalFileName;
    end;

    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socSfacFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgLandUse.Cells[Ord(socSfacFile), ARow] := FarmProperty.ExternalScaleFileName;
    end;
  end;
begin
  cpnlPrint.Collapse;
  if cpnlgrp1.VertScrollBar.Visible then
  begin
    cpnlgrp1.VertScrollBar.Position := 0;
  end;

  inherited;
  LandUsePackage := Package as TFarmProcess4LandUse;

  for PrintIndex := Low(TLandUsePrint) to High(TLandUsePrint) do
  begin
    clbPrint.Checked[Ord(PrintIndex)] := PrintIndex in LandUsePackage.LandUsePrints;
  end;

  comboLandUsePerCell.ItemIndex := Ord(LandUsePackage.LandUseOption);
  rdeMinimumBareFraction.RealValue := LandUsePackage.MinimumBareFraction;
  rdeRelaxFracHeadChange.RealValue := LandUsePackage.RelaxFracHeadChange;

  rdgLandUse.BeginUpdate;
  try
    rdgLandUse.Cells[Ord(socTransient), Ord(sorSoilLocation)] := StaticTransient[Ord(LandUsePackage.CropLocation)];

    GetFarmProperty(LandUsePackage.LandUseFraction, Ord(sorLandUseFraction));
    GetFarmOption(comboSpecifyCrops, LandUsePackage.SpecifyCropsToPrint);
    GetFarmProperty(LandUsePackage.CropCoeff, Ord(sorCropCoeff));
    GetFarmProperty(LandUsePackage.ConsumptiveUse, Ord(sorConsumptiveUse));
    GetFarmProperty(LandUsePackage.Irrigation, Ord(sorIrrigation));
    GetFarmProperty(LandUsePackage.RootDepth, Ord(sorRootDepth));
    GetFarmProperty(LandUsePackage.RootPressure, Ord(sorRootPressure));
    GetFarmProperty(LandUsePackage.GroundwaterRootInteraction, Ord(sorGroundwaterRootInteraction));
    GetFarmProperty(LandUsePackage.TranspirationFraction, Ord(sorTranspirationFraction));
    GetFarmProperty(LandUsePackage.EvapIrrigationFraction, Ord(sorEvapIrrigationFraction));
    GetFarmProperty(LandUsePackage.FractionOfPrecipToSurfaceWater, Ord(sorFractionOfPrecipToSurfaceWater));
    GetFarmProperty(LandUsePackage.FractionOfIrrigationToSurfaceWater, Ord(sorFractionOfIrrigationToSurfaceWater));
    GetFarmProperty(LandUsePackage.PondDepth, Ord(sorPondDepth));
    GetFarmProperty(LandUsePackage.AddedDemand, Ord(sorAddedDemand));
    GetFarmProperty(LandUsePackage.NoCropUseMeansBareSoil, Ord(sorNoCropUseMeansBareSoil));
    GetFarmProperty(LandUsePackage.ET_IrrigFracCorrection, Ord(sorET_IrrigFracCorrection));

    rdgLandUse.Cells[Ord(socOther), Ord(sorEvapIrrigationFraction)] :=
      ByCropIrrigate[Ord(LandUsePackage.EvapIrrigationOption)];
    rdgLandUse.Cells[Ord(socOther), Ord(sorFractionOfIrrigationToSurfaceWater)] :=
      ByCropIrrigate[Ord(LandUsePackage.FractionOfPrecipToSurfaceWaterIrrigationOption)];
    rdgLandUse.Cells[Ord(socOther), Ord(sorAddedDemand)] :=
      LengthRate[Ord(LandUsePackage.AddedDemandOption)];

  finally
    rdgLandUse.EndUpdate;
  end;
end;

procedure TframePackageFmp4LandUse.InitializeGrid;
begin
  rdgLandUse.BeginUpdate;
  try
    rdgLandUse.FixedCols := 1;

    rdgLandUse.Cells[Ord(socTransient), Ord(sorName)] := StrFrequency;
    rdgLandUse.Cells[Ord(socArray), Ord(sorName)] := StrArrayOrList;
    rdgLandUse.Cells[Ord(socOther), Ord(sorName)] := StrOption;

    rdgLandUse.Cells[Ord(socSFAC), Ord(sorName)] := StrUnitConversionScal;
    rdgLandUse.Cells[Ord(socFile), Ord(sorName)] := StrExternallyGenerated;
    rdgLandUse.Cells[Ord(socSfacFile), Ord(sorName)] := StrExternallyGeneratedSfac;

    rdgLandUse.Cells[Ord(socName), Ord(sorSoilLocation)] := StrCropLocation;
    rdgLandUse.Cells[Ord(socName), Ord(sorLandUseFraction)] := StrLandUseFraction;
    rdgLandUse.Cells[Ord(socName), Ord(sorCropCoeff)] := StrCropCoeff;
    rdgLandUse.Cells[Ord(socName), Ord(sorConsumptiveUse)] := StrConsumptiveUse;
    rdgLandUse.Cells[Ord(socName), Ord(sorIrrigation)] := StrIrrigation;
    rdgLandUse.Cells[Ord(socName), Ord(sorRootDepth)] := StrRootDepth;
    rdgLandUse.Cells[Ord(socName), Ord(sorRootPressure)] := StrRootPressure;
    rdgLandUse.Cells[Ord(socName), Ord(sorGroundwaterRootInteraction)] := StrGroundwaterRootInt;
    rdgLandUse.Cells[Ord(socName), Ord(sorTranspirationFraction)] := StrTranspirationFracti;
    rdgLandUse.Cells[Ord(socName), Ord(sorEvapIrrigationFraction)] := StrEvapIrrigFraction;
    rdgLandUse.Cells[Ord(socName), Ord(sorFractionOfPrecipToSurfaceWater)] := StrFractionOfPrecip;
    rdgLandUse.Cells[Ord(socName), Ord(sorFractionOfIrrigationToSurfaceWater)] := StrFractionOfIrrigT;
    rdgLandUse.Cells[Ord(socName), Ord(sorPondDepth)] := StrPondDepth;
    rdgLandUse.Cells[Ord(socName), Ord(sorAddedDemand)] := StrAddedDemand;
    rdgLandUse.Cells[Ord(socName), Ord(sorNoCropUseMeansBareSoil)] := StrNoCropMeansBareS;
    rdgLandUse.Cells[Ord(socName), Ord(sorET_IrrigFracCorrection)] := StrETIrrigFracCorr;
  finally
    rdgLandUse.EndUpdate;
  end;
end;

procedure TframePackageFmp4LandUse.Loaded;
begin
  inherited;
  cpnlPrint.Collapse;
  cpnlOptions.Collapse;
  InitializeGrid;
end;

procedure TframePackageFmp4LandUse.rdgLandUseSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Column: TRbwColumn4;
  SoilRow: TSoilOptionRows;
  SoilColumns: TSoilOptionColumns;
begin
  inherited;
  if (ACol = Ord(socTransient)) and not rdgLandUse.Drawing then
  begin
    Column := rdgLandUse.Columns[Ord(socTransient)];
    if (ARow = Ord(sorSoilLocation)) then
    begin
      Column.PickList := StaticTransient;
    end
    else if ARow = Ord(sorGroundwaterRootInteraction) then
    begin
      Column.PickList := DontUseStatic;
    end
    else
    begin
      Column.PickList := DontUseStaticTransient;
    end;
  end;

  if (ACol = Ord(socOther)) and not rdgLandUse.Drawing then
  begin
    Column := rdgLandUse.Columns[Ord(socOther)];
    if ARow in [Ord(sorEvapIrrigationFraction), Ord(sorFractionOfIrrigationToSurfaceWater)] then
    begin
      Column.PickList := ByCropIrrigate;
    end
    else if ARow in [Ord(sorAddedDemand)] then
    begin
      Column.PickList := LengthRate
    end;
  end;

  SoilColumns := TSoilOptionColumns(ACol);
  SoilRow := TSoilOptionRows(ARow);
  case SoilColumns of
    socTransient: ;
    socArray:
      begin
        CanSelect := SoilRow in [sorLandUseFraction, sorCropCoeff,
          sorConsumptiveUse, sorIrrigation, sorRootDepth,
          sorGroundwaterRootInteraction, sorTranspirationFraction,
          sorEvapIrrigationFraction, sorFractionOfPrecipToSurfaceWater,
          sorFractionOfIrrigationToSurfaceWater, sorAddedDemand];
      end;
    socSFAC, socSfacFile:
      begin
        CanSelect := SoilRow in [sorLandUseFraction, sorCropCoeff,
          sorConsumptiveUse, sorRootDepth, sorTranspirationFraction,
          sorEvapIrrigationFraction, sorFractionOfPrecipToSurfaceWater,
          sorFractionOfIrrigationToSurfaceWater, sorPondDepth, sorAddedDemand]
      end;
    socOther:
      begin
        if (SoilRow in [sorEvapIrrigationFraction,
          sorFractionOfIrrigationToSurfaceWater]) then
        begin
          CanSelect := (rdgLandUse.ItemIndex[Ord(socArray), ARow] = 1)
            and (rdgLandUse.ItemIndex[Ord(socTransient), ARow] > 0)
        end
        else if SoilRow = sorAddedDemand then
        begin
          CanSelect := (rdgLandUse.ItemIndex[Ord(socTransient), ARow] > 0);
        end
        else
        begin
          CanSelect := False;
        end;
      end;
  end;
  if (SoilRow = sorSoilLocation) and (comboLandUsePerCell.ItemIndex <> 0) then
  begin
    CanSelect := False;
  end;
  if (SoilColumns in [socArray, socSFAC, socFile, socSfacFile])
    and (ARow >= Ord(sorConsumptiveUse))
    and (rdgLandUse.ItemIndex[Ord(socTransient), ARow] <= 0) then
  begin
    CanSelect := False;
  end;
end;

procedure TframePackageFmp4LandUse.SetData(Package: TModflowPackageSelection);
  function SetFarmOptionGrid(Row: TSoilOptionRows): TFarmOption;
  var
    Index: Integer;
  begin
    Index := DontUseStaticTransient.IndexOf(rdgLandUse.Cells[Ord(socTransient), Ord(Row)]);
    if Index < 0 then
    begin
      Index := 0
    end;
    Result := TFarmOption(Index);
  end;
  function SetArrayListGrid(Row: TSoilOptionRows): TArrayList;
  var
    ItemIndex: Integer;
  begin
    ItemIndex := rdgLandUse.ItemIndex[Ord(socArray), Ord(Row)];
    if ItemIndex < 0 then
    begin
      ItemIndex := 0;
    end;
    result := TArrayList(ItemIndex);
  end;
  procedure SetFarmProperty(FarmProperty: TFarmProperty; ARow: TSoilOptionRows);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socTransient), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.FarmOption := SetFarmOptionGrid(ARow);
    end;

    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socArray), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ArrayList := SetArrayListGrid(ARow);
    end;

    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socSFAC), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.UnitConversionScaleFactor :=
        rdgLandUse.Cells[Ord(socSFAC), Ord(ARow)];
    end;

    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalFileName :=
        rdgLandUse.Cells[Ord(socFile), Ord(ARow)];
    end;

    CanSelect := True;
    rdgLandUseSelectCell(rdgLandUse, Ord(socSfacFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalScaleFileName :=
        rdgLandUse.Cells[Ord(socSfacFile), Ord(ARow)];
    end;
  end;
var
  LandUsePackage: TFarmProcess4LandUse;
  PrintIndex: TLandUsePrint;
  PrintChoices: TLandUsePrints;
begin
  inherited;

  LandUsePackage := Package as TFarmProcess4LandUse;

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

  LandUsePackage.CropLocation := TRequiredSteadyTransient(
    StaticTransient.IndexOf(rdgLandUse.Cells[Ord(socTransient), Ord(sorSoilLocation)]));

  SetFarmProperty(LandUsePackage.LandUseFraction, sorLandUseFraction);
  if (LandUsePackage.LandUseOption = luoMultiple)
    and (LandUsePackage.LandUseFraction.FarmOption = foNotUsed) then
  begin
    LandUsePackage.LandUseFraction.FarmOption := foStatic
  end;
  LandUsePackage.SpecifyCropsToPrint := SetFarmOption(comboSpecifyCrops);
  SetFarmProperty(LandUsePackage.CropCoeff, sorCropCoeff);
  SetFarmProperty(LandUsePackage.ConsumptiveUse, sorConsumptiveUse);
  SetFarmProperty(LandUsePackage.Irrigation, sorIrrigation);
  SetFarmProperty(LandUsePackage.RootDepth, sorRootDepth);
  SetFarmProperty(LandUsePackage.RootPressure, sorRootPressure);
  SetFarmProperty(LandUsePackage.GroundwaterRootInteraction, sorGroundwaterRootInteraction);

  SetFarmProperty(LandUsePackage.TranspirationFraction, sorTranspirationFraction);
  SetFarmProperty(LandUsePackage.EvapIrrigationFraction, sorEvapIrrigationFraction);
  SetFarmProperty(LandUsePackage.FractionOfPrecipToSurfaceWater, sorFractionOfPrecipToSurfaceWater);
  SetFarmProperty(LandUsePackage.FractionOfIrrigationToSurfaceWater, sorFractionOfIrrigationToSurfaceWater);
  SetFarmProperty(LandUsePackage.PondDepth, sorPondDepth);
  SetFarmProperty(LandUsePackage.AddedDemand, sorAddedDemand);
  SetFarmProperty(LandUsePackage.NoCropUseMeansBareSoil, sorNoCropUseMeansBareSoil);
  SetFarmProperty(LandUsePackage.ET_IrrigFracCorrection, sorET_IrrigFracCorrection);

  LandUsePackage.EvapIrrigationOption := TIrrigationOption(ByCropIrrigate.IndexOf
    (rdgLandUse.Cells[Ord(socOther), Ord(sorEvapIrrigationFraction)]));
  LandUsePackage.FractionOfPrecipToSurfaceWaterIrrigationOption := TIrrigationOption(ByCropIrrigate.IndexOf
    (rdgLandUse.Cells[Ord(socOther), Ord(sorFractionOfIrrigationToSurfaceWater)]));
  LandUsePackage.AddedDemandOption := TDemandOption(LengthRate.IndexOf
    (rdgLandUse.Cells[Ord(socOther), Ord(sorAddedDemand)]));

end;

initialization
  ByCropIrrigate := TStringList.Create;
  ByCropIrrigate.Add('By Crop');
  ByCropIrrigate.Add('By Irrigate');

  LengthRate := TStringList.Create;
  LengthRate.Add('Length (L/T)');
  LengthRate.Add('Rate (L^3/T)');

finalization
  ByCropIrrigate.Free;
  LengthRate.Free;

end.
