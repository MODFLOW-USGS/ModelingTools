unit framePackageFmp4Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, ArgusDataEntry, Vcl.Grids,
  RbwDataGrid4, ModflowPackageSelectionUnit;

type
  TFarm4Columns = (fcName, fcTransient, fcArray, fcOther);
  TFarm4Rows = (frName, frLocation, frEfficiency, frEfficiencyImprove,
    frDeficiency, frWaterSource, frBareRunnoffFraction, frBarePrecip,
    frAddedDemandSplit, frAddedDemandFlux, frAddedDemandRate);

  TframePackageFmp4 = class(TframePackage)
    CategoryPanelGroup1: TCategoryPanelGroup;
    cpnlOutput: TCategoryPanel;
    clbPrint: TCheckListBox;
    Panel1: TPanel;
    comboPrintRouting: TComboBox;
    lblPrintRouting: TLabel;
    cpnlMnw2: TCategoryPanel;
    cbMnwClose: TCheckBox;
    rdeQClose: TRbwDataEntry;
    rdeHPCT: TRbwDataEntry;
    rdeRPCT: TRbwDataEntry;
    lblQClose: TLabel;
    lblHPCT: TLabel;
    lblRPCT: TLabel;
    cpnlOptions: TCategoryPanel;
    cbAllowPrinting: TCheckBox;
    cbWellField: TCheckBox;
    cbRecompute: TCheckBox;
    cpnlWaterBalanceRegions: TCategoryPanel;
    rdgFarms: TRbwDataGrid4;
    procedure rdgFarmsSelectCell(Sender: TObject; ACol, ARow: Integer;
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
  framePackageFmp4: TframePackageFmp4;

implementation

resourcestring
  StrFrequency = 'Frequency';
  StrArrayOrList = 'Array or list';

{$R *.dfm}

{ TframePackageFmp4 }

procedure TframePackageFmp4.GetData(Package: TModflowPackageSelection);
var
  FarmProcess4: TFarmProcess4;
  PrintIndex: TFarmPrint;
begin
  cpnlMnw2.Collapse;
  cpnlOptions.Collapse;
  cpnlOutput.Collapse;
//  cpnlWaterBalanceRegions.Collapsed := False;

  inherited;
  FarmProcess4 := Package as TFarmProcess4;

  for PrintIndex := Low(TFarmPrint) to High(TFarmPrint) do
  begin
    clbPrint.Checked[Ord(PrintIndex)] := PrintIndex in FarmProcess4.FarmPrints;
  end;

  cbMnwClose.Checked := FarmProcess4.UseMnwCriteria;
  rdeQClose.RealValue := FarmProcess4.MnwQClose;
  rdeHPCT.RealValue := FarmProcess4.MnwHPercent;
  rdeRPCT.RealValue := FarmProcess4.MnwRPercent;

  cbAllowPrinting.Checked := FarmProcess4.Print;
  cbWellField.Checked := FarmProcess4.WELLFIELD;
  cbRecompute.Checked := FarmProcess4.Recompute;

  rdgFarms.BeginUpdate;
  try
    rdgFarms.Cells[Ord(fcTransient), Ord(frLocation)] := StaticTransient[Ord(FarmProcess4.TransientFarms)];

    rdgFarms.Cells[Ord(fcTransient), Ord(frEfficiency)] := DontUseStaticTransient[Ord(FarmProcess4.EfficiencyOption)];
    rdgFarms.ItemIndex[Ord(fcArray), Ord(frEfficiency)] := Ord(FarmProcess4.EfficiencyArrayList);

    rdgFarms.Cells[Ord(fcTransient), Ord(frEfficiencyImprove)] := DontUseStaticTransient[Ord(FarmProcess4.EfficiencyImprovement)];
    rdgFarms.ItemIndex[Ord(fcArray), Ord(frEfficiencyImprove)] := Ord(FarmProcess4.EfficiencyImprovementArrayList);

    rdgFarms.Cells[Ord(fcTransient), Ord(frDeficiency)] := DontUseStaticTransient[Ord(FarmProcess4.DeficiencyScenario)];
    rdgFarms.ItemIndex[Ord(fcOther), Ord(frDeficiency)] := Ord(FarmProcess4.ProrateDeficiency);

    rdgFarms.Cells[Ord(fcTransient), Ord(frWaterSource)] := DontUseStaticTransient[Ord(FarmProcess4.Watersource)];

    rdgFarms.Cells[Ord(fcTransient), Ord(frBareRunnoffFraction)] := DontUseStaticTransient[Ord(FarmProcess4.Bare_Runoff_Fraction)];
    rdgFarms.ItemIndex[Ord(fcArray), Ord(frBareRunnoffFraction)] := Ord(FarmProcess4.Bare_Runoff_FractionArrayList);

    rdgFarms.Cells[Ord(fcTransient), Ord(frBarePrecip)] := DontUseStaticTransient[Ord(FarmProcess4.Bare_Precipitation_Consumption_Fraction)];

    rdgFarms.Cells[Ord(fcTransient), Ord(frAddedDemandSplit)] := DontUseStaticTransient[Ord(FarmProcess4.Added_Demand_Runoff_Split)];
    rdgFarms.ItemIndex[Ord(fcArray), Ord(frAddedDemandSplit)] := Ord(FarmProcess4.Added_Demand_Runoff_SplitArrayList);

    rdgFarms.Cells[Ord(fcTransient), Ord(frAddedDemandFlux)] := DontUseStaticTransient[Ord(FarmProcess4.Added_Crop_Demand_Flux)];

    rdgFarms.Cells[Ord(fcTransient), Ord(frAddedDemandRate)] := DontUseStaticTransient[Ord(FarmProcess4.Added_Crop_Demand_Rate)];
  finally
    rdgFarms.EndUpdate;
  end;

end;

procedure TframePackageFmp4.InitializeGrid;
begin
  rdgFarms.BeginUpdate;
  try
    rdgFarms.Cells[Ord(fcTransient), Ord(frName)] := StrFrequency;
    rdgFarms.Cells[Ord(fcArray), Ord(frName)] := StrArrayOrList;
    rdgFarms.Cells[Ord(fcOther), Ord(frName)] := 'Prorate Deficiency';

    rdgFarms.Cells[Ord(fcName), Ord(frLocation)] := 'Location';
    rdgFarms.Cells[Ord(fcName), Ord(frEfficiency)] := 'Efficiency';
    rdgFarms.Cells[Ord(fcName), Ord(frEfficiencyImprove)] := 'Efficiency_Improvement';
    rdgFarms.Cells[Ord(fcName), Ord(frDeficiency)] := 'Deficiency_Scenario';
    rdgFarms.Cells[Ord(fcName), Ord(frWaterSource)] := 'Watersource';
    rdgFarms.Cells[Ord(fcName), Ord(frBareRunnoffFraction)] := 'Bare_Runoff_Fraction';
    rdgFarms.Cells[Ord(fcName), Ord(frBarePrecip)] := 'Bare_Precipitation_Consumption_Fraction';
    rdgFarms.Cells[Ord(fcName), Ord(frAddedDemandSplit)] := 'Added_Demand_Runoff_Split';
    rdgFarms.Cells[Ord(fcName), Ord(frAddedDemandFlux)] := 'Added_Crop_Demand Flux';
    rdgFarms.Cells[Ord(fcName), Ord(frAddedDemandRate)] := 'Added_Crop_Demand Rate';
  finally
    rdgFarms.EndUpdate;
  end;
end;

procedure TframePackageFmp4.Loaded;
begin
  inherited;
  InitializeGrid;
end;

procedure TframePackageFmp4.rdgFarmsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Column: TRbwColumn4;
  FarmRow: TFarm4Rows;
begin
  inherited;
  if (ACol = Ord(fcTransient)) and not rdgFarms.Drawing then
  begin
    Column := rdgFarms.Columns[Ord(fcTransient)];
    if (ARow = Ord(frLocation)) then
    begin
      Column.PickList := StaticTransient;
    end
    else
    begin
      Column.PickList := DontUseStaticTransient;
    end;
  end;

  if (ACol = Ord(fcArray)) then
  begin
    FarmRow := TFarm4Rows(ARow);
    CanSelect := FarmRow in [frEfficiency, frEfficiencyImprove,
      frBareRunnoffFraction, frAddedDemandSplit];
  end;

  if (ACol = Ord(fcOther)) then
  begin
    FarmRow := TFarm4Rows(ARow);
    CanSelect := FarmRow = frDeficiency;
  end;
end;

procedure TframePackageFmp4.SetData(Package: TModflowPackageSelection);
var
  FarmProcess4: TFarmProcess4;
  FarmPrints: TFarmPrints;
  PrintIndex: TFarmPrint;
  function RowToFarmOption(ARow: TFarm4Rows): TFarmOption;
  begin
    result := TFarmOption(DontUseStaticTransient.IndexOf(rdgFarms.Cells[Ord(fcTransient), Ord(ARow)]));
  end;
  function RowToArrayList(ARow: TFarm4Rows): TArrayList;
  begin
    result := TArrayList(rdgFarms.ItemIndex[Ord(fcArray), Ord(ARow)]);
  end;

begin
  inherited;
  FarmProcess4 := Package as TFarmProcess4;

  FarmPrints := [];
  for PrintIndex := Low(TFarmPrint) to High(TFarmPrint) do
  begin
    if clbPrint.Checked[Ord(PrintIndex)] then
    begin
      Include(FarmPrints, PrintIndex)
    end;
  end;
  FarmProcess4.FarmPrints := FarmPrints;

  FarmProcess4.UseMnwCriteria := cbMnwClose.Checked;
  FarmProcess4.MnwQClose := rdeQClose.RealValue;
  FarmProcess4.MnwHPercent := rdeHPCT.RealValue;
  FarmProcess4.MnwRPercent := rdeRPCT.RealValue;

  FarmProcess4.Print := cbAllowPrinting.Checked;
  FarmProcess4.WELLFIELD := cbWellField.Checked;
  FarmProcess4.Recompute := cbRecompute.Checked;

  FarmProcess4.TransientFarms := Boolean(StaticTransient.IndexOf(rdgFarms.Cells[Ord(fcTransient), Ord(frLocation)]));

  FarmProcess4.EfficiencyOption := RowToFarmOption(frEfficiency);
  FarmProcess4.EfficiencyArrayList := RowToArrayList(frEfficiency);

  FarmProcess4.EfficiencyImprovement := RowToFarmOption(frEfficiencyImprove);
  FarmProcess4.EfficiencyImprovementArrayList := RowToArrayList(frEfficiencyImprove);

  FarmProcess4.DeficiencyScenario := RowToFarmOption(frDeficiency);
  FarmProcess4.ProrateDeficiency := TProrateDeficiencyOption(rdgFarms.ItemIndex[Ord(fcOther), Ord(frDeficiency)]);

  FarmProcess4.Watersource := RowToFarmOption(frWaterSource);

  FarmProcess4.Bare_Runoff_Fraction := RowToFarmOption(frBareRunnoffFraction);
  FarmProcess4.Bare_Runoff_FractionArrayList := RowToArrayList(frBareRunnoffFraction);

  FarmProcess4.Bare_Precipitation_Consumption_Fraction := RowToFarmOption(frBarePrecip);

  FarmProcess4.Added_Demand_Runoff_Split := RowToFarmOption(frAddedDemandSplit);
  FarmProcess4.Added_Demand_Runoff_SplitArrayList := RowToArrayList(frAddedDemandSplit);

  FarmProcess4.Added_Crop_Demand_Flux := RowToFarmOption(frAddedDemandFlux);

  FarmProcess4.Added_Crop_Demand_Rate := RowToFarmOption(frAddedDemandRate);
end;

end.
