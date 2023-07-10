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
begin
  rdgChemDensity.BeginUpdate;
  try
    if MobileComponents.Count > 0 then
    begin
      rdgChemDensity.RowCount := MobileComponents.Count + 1;
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

    if MobileComponents.Count > 0 then
    begin
      for ChemIndex := 0 to MobileComponents.Count - 1 do
      begin
        AChemSpecies := MobileComponents[ChemIndex];
        rdgChemDensity.Cells[Ord(ccName), ChemIndex+1] := AChemSpecies.Name;
        rdgChemDensity.RealValue[Ord(ccRefConcentration), ChemIndex+1] := AChemSpecies.RefConcentration;
        rdgChemDensity.RealValue[Ord(ccSlope), ChemIndex+1] := AChemSpecies.DensitySlope;
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
  AVAlue: Double;
begin
    for ChemIndex := 0 to MobileComponents.Count - 1 do
    begin
      if ChemIndex + 1  >= rdgChemDensity.RowCount then
      begin
         Break;
      end;
      AChemSpecies := MobileComponents[ChemIndex];

      if TryStrToFloat(rdgChemDensity.Cells[Ord(ccRefConcentration), ChemIndex+1], AVAlue) then
      begin
        AChemSpecies.RefConcentration := AVAlue;
      end;
      if TryStrToFloat(rdgChemDensity.Cells[Ord(ccSlope), ChemIndex+1], AVAlue) then
      begin
        AChemSpecies.DensitySlope := AVAlue;
      end;
    end;
end;

end.
