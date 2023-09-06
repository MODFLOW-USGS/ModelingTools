unit framePackageViscosityUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ArgusDataEntry, ModflowPackageSelectionUnit, Mt3dmsChemSpeciesUnit,
  Vcl.Grids, RbwDataGrid4;

type
  TChemViscColumns = (ccvName, ccvRefConcentration, ccvSlope);

  TframePackageViscosity = class(TframePackage)
    cbSpecifyViscosity: TCheckBox;
    rdeRefViscosity: TRbwDataEntry;
    lblRefViscosity: TLabel;
    cbWriteViscosity: TCheckBox;
    comboThermalSpecies: TComboBox;
    lblThermalSpecies: TLabel;
    comboThermalFormulation: TComboBox;
    lblThermalFormulation: TLabel;
    rdeThermalA2: TRbwDataEntry;
    lblThermalA2: TLabel;
    rdeThermalA3: TRbwDataEntry;
    lblThermalA3: TLabel;
    rdeThermalA4: TRbwDataEntry;
    lblThermalA4: TLabel;
    rdgChemViscosity: TRbwDataGrid4;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    // @name should be called before @link(GetData).
    procedure GetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection);
    procedure SetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection);
    { Public declarations }
  end;

var
  framePackageViscosity: TframePackageViscosity;

implementation

uses
  frmCustomGoPhastUnit;

{$R *.dfm}

{ TframePackageViscosity }

procedure TframePackageViscosity.GetData(Package: TModflowPackageSelection);
var
  Vicosity: TViscosityPackage;
  ItemIndex: Integer;
begin
  inherited;
  Vicosity := Package as TViscosityPackage;
  cbSpecifyViscosity.Checked := Vicosity.ViscositySpecified;
  cbWriteViscosity.Checked := Vicosity.WriteViscosity;
  rdeRefViscosity.RealValue := Vicosity.RefViscosity;
  if Vicosity.ThermalSpecies = '' then
  begin
    comboThermalSpecies.ItemIndex := 0;
  end
  else
  begin
    ItemIndex := comboThermalSpecies.Items.IndexOf(Vicosity.ThermalSpecies);
    if ItemIndex < 0 then
    begin
      ItemIndex := 0;
    end;
    comboThermalSpecies.ItemIndex := ItemIndex;
  end;
  comboThermalFormulation.ItemIndex := Ord(Vicosity.ThermalFormulation);
  rdeThermalA2.RealValue := Vicosity.ThermalA2;
  rdeThermalA3.RealValue := Vicosity.ThermalA3;
  rdeThermalA4.RealValue := Vicosity.ThermalA4;
end;

procedure TframePackageViscosity.GetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection);
var
  ChemIndex: Integer;
  AChemSpecies: TMobileChemSpeciesItem;
  Index: Integer;
  UsedSpeciesCount: Integer;
  RowIndex: Integer;
begin
  comboThermalSpecies.Items.BeginUpdate;
  rdgChemViscosity.BeginUpdate;
  try
    comboThermalSpecies.Items.Clear;
    comboThermalSpecies.Items.Add('none');
    UsedSpeciesCount := 0;
    for Index := 0 to MobileComponents.Count - 1 do
    begin
      if MobileComponents[Index].UsedForGWT then
      begin
        Inc(UsedSpeciesCount)
      end;
    end;
    comboThermalSpecies.Items.Capacity := UsedSpeciesCount;

    if UsedSpeciesCount > 0 then
    begin
      rdgChemViscosity.RowCount := UsedSpeciesCount + 1;
    end
    else
    begin
      rdgChemViscosity.RowCount := 2;
    end;
    rdgChemViscosity.FixedRows := 1;
    rdgChemViscosity.FixedCols := 1;
    ClearGrid(rdgChemViscosity);

    rdgChemViscosity.Cells[Ord(ccvName), 0] := 'Chem. Species';
    rdgChemViscosity.Cells[Ord(ccvRefConcentration), 0] := 'Reference Concentration or Temperature (cviscref)';
    rdgChemViscosity.Cells[Ord(ccvSlope), 0] := 'Slope (dviscdc)';

    RowIndex := 1;
    if MobileComponents.Count > 0 then
    begin
      for ChemIndex := 0 to MobileComponents.Count - 1 do
      begin
        AChemSpecies := MobileComponents[ChemIndex];
        if AChemSpecies.UsedForGWT then
        begin
          comboThermalSpecies.Items.AddObject(AChemSpecies.Name, AChemSpecies);

          rdgChemViscosity.Cells[Ord(ccvName), RowIndex] := AChemSpecies.Name;
          rdgChemViscosity.RealValue[Ord(ccvRefConcentration), RowIndex] := AChemSpecies.RefViscosity;
          rdgChemViscosity.RealValue[Ord(ccvSlope), RowIndex] := AChemSpecies.ViscositySlope;
          Inc(RowIndex);
        end;
      end;
    end;
  finally
    rdgChemViscosity.EndUpdate;
    comboThermalSpecies.Items.EndUpdate;
  end;
end;

procedure TframePackageViscosity.SetData(Package: TModflowPackageSelection);
var
  Vicosity: TViscosityPackage;
begin
  inherited;
  Vicosity := Package as TViscosityPackage;
  Vicosity.ViscositySpecified := cbSpecifyViscosity.Checked;
  Vicosity.WriteViscosity := cbWriteViscosity.Checked;
  Vicosity.RefViscosity := rdeRefViscosity.RealValueDefault(1.0);
  if comboThermalSpecies.ItemIndex = 0 then
  begin
    Vicosity.ThermalSpecies := ''
  end
  else
  begin
    Vicosity.ThermalSpecies := comboThermalSpecies.Text;
  end;
  Vicosity.ThermalFormulation := TThermalFormulation(comboThermalFormulation.ItemIndex);
  Vicosity.ThermalA2 := rdeThermalA2.RealValueDefault(10.0);
  Vicosity.ThermalA3 := rdeThermalA3.RealValueDefault(248.37);
  Vicosity.ThermalA4 := rdeThermalA4.RealValueDefault(133.15);
end;

procedure TframePackageViscosity.SetMt3dmsChemSpecies(
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
      if RowIndex >= rdgChemViscosity.RowCount then
      begin
         Break;
      end;

      if TryStrToFloat(rdgChemViscosity.Cells[Ord(ccvRefConcentration), RowIndex], AValue) then
      begin
        AChemSpecies.RefViscosity := AValue;
      end;
      if TryStrToFloat(rdgChemViscosity.Cells[Ord(ccvSlope), RowIndex], AValue) then
      begin
        AChemSpecies.ViscositySlope := AValue;
      end;
      Inc(RowIndex)
    end;
  end;
end;

end.
