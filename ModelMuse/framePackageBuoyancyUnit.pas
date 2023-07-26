unit framePackageBuoyancyUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.Grids, RbwDataGrid4, ModflowPackageSelectionUnit, Mt3dmsChemSpeciesUnit,
  ArgusDataEntry;

type
  TChemColumns = (ccName, ccRefConcentration, ccSlope);

  TframePackageBuoyancy = class(TframePackage)
    cbSpecifyDensity: TCheckBox;
    cbRHS: TCheckBox;
    cbWriteDensity: TCheckBox;
    rdgChemDensity: TRbwDataGrid4;
    rdeRefDensity: TRbwDataEntry;
    lblRefDensity: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    procedure GetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection);
    procedure SetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection);
    { Public declarations }
  end;

var
  framePackageBuoyancy: TframePackageBuoyancy;

implementation

uses
  frmCustomGoPhastUnit;

{$R *.dfm}

{ TframePackageBuoyancy }

procedure TframePackageBuoyancy.GetData(Package: TModflowPackageSelection);
var
  Buoyancy: TBuoyancyPackage;
begin
  inherited;
  Buoyancy := Package as TBuoyancyPackage;
  cbSpecifyDensity.Checked := Buoyancy.DensityUsed;
  cbRHS.Checked := Buoyancy.RightHandSide;
  rdeRefDensity.RealValue := Buoyancy.RefDensity;
  cbWriteDensity.Checked := Buoyancy.WriteDensity;
end;

procedure TframePackageBuoyancy.GetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection);
var
  ChemIndex: Integer;
  AChemSpecies: TMobileChemSpeciesItem;
  Index: Integer;
  UsedSpeciesCount: Integer;
  RowIndex: Integer;
begin
  rdgChemDensity.BeginUpdate;
  try
    UsedSpeciesCount := 0;
    for Index := 0 to MobileComponents.Count - 1 do
    begin
      if MobileComponents[Index].UsedForGWT then
      begin
        Inc(UsedSpeciesCount)
      end;
    end;

    if UsedSpeciesCount > 0 then
    begin
      rdgChemDensity.RowCount := UsedSpeciesCount + 1;
    end
    else
    begin
      rdgChemDensity.RowCount := 2;
    end;
    rdgChemDensity.FixedRows := 1;
    rdgChemDensity.FixedCols := 1;
    ClearGrid(rdgChemDensity);

    rdgChemDensity.Cells[Ord(ccName), 0] := 'Chem. Species';
    rdgChemDensity.Cells[Ord(ccRefConcentration), 0] := 'Reference Concentration (crhoref)';
    rdgChemDensity.Cells[Ord(ccSlope), 0] := 'Slope (drhodc)';

    RowIndex := 1;
    if MobileComponents.Count > 0 then
    begin
      for ChemIndex := 0 to MobileComponents.Count - 1 do
      begin
        AChemSpecies := MobileComponents[ChemIndex];
        if AChemSpecies.UsedForGWT then
        begin
          rdgChemDensity.Cells[Ord(ccName), RowIndex] := AChemSpecies.Name;
          rdgChemDensity.RealValue[Ord(ccRefConcentration), RowIndex] := AChemSpecies.RefConcentration;
          rdgChemDensity.RealValue[Ord(ccSlope), RowIndex] := AChemSpecies.DensitySlope;
          Inc(RowIndex);
        end;
      end;
    end;
  finally
    rdgChemDensity.EndUpdate;
  end;
end;

procedure TframePackageBuoyancy.SetData(Package: TModflowPackageSelection);
var
  Buoyancy: TBuoyancyPackage;
  AValue: Double;
begin
  inherited;
  Buoyancy := Package as TBuoyancyPackage;
  Buoyancy.DensityUsed := cbSpecifyDensity.Checked;
  Buoyancy.RightHandSide := cbRHS.Checked;
  if rdeRefDensity.TryGetRealValue(AValue) then
  begin
    Buoyancy.RefDensity := AValue;
  end;
  Buoyancy.WriteDensity := cbWriteDensity.Checked;
end;

procedure TframePackageBuoyancy.SetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection);
var
  ChemIndex: Integer;
  AChemSpecies: TMobileChemSpeciesItem;
  AValue: Double;
  RowIndex: Integer;
begin
  RowIndex := 1;
  for ChemIndex := 0 to MobileComponents.Count - 1 do
  begin
    AChemSpecies := MobileComponents[ChemIndex];
    if AChemSpecies.UsedForGWT then
    begin
      if RowIndex >= rdgChemDensity.RowCount then
      begin
         Break;
      end;

      if TryStrToFloat(rdgChemDensity.Cells[Ord(ccRefConcentration), RowIndex], AValue) then
      begin
        AChemSpecies.RefConcentration := AValue;
      end;
      if TryStrToFloat(rdgChemDensity.Cells[Ord(ccSlope), RowIndex], AValue) then
      begin
        AChemSpecies.DensitySlope := AValue;
      end;
      Inc(RowIndex)
    end;
  end;
end;

end.
