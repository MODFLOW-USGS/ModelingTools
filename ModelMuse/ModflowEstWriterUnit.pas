unit ModflowEstWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowGwtMstWriter = class(TCustomPackageWriter)
  private
    FSpeciesIndex: Integer;
    FEstPackage: TGweEstPackage;
    FPestScriptName: string;
    procedure WriteOptions;
    procedure WriteGridData;
    procedure WriteFileInternal;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(AFileName: string; SpeciesIndex: Integer);
  end;


implementation

uses
  DataSetUnit, Mt3dmsChemSpeciesUnit, PhastModelUnit, DataSetNamesUnit;

{ TModflowGwtMstWriter }

class function TModflowGwtMstWriter.Extension: string;
begin
  result := '.est';
end;

function TModflowGwtMstWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GweEstPackage;
end;

procedure TModflowGwtMstWriter.WriteFile(AFileName: string;
  SpeciesIndex: Integer);
var
  Abbreviation: string;
  GwtFile: string;
begin
  FSpeciesIndex := SpeciesIndex;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GweUsed then
  begin
    Exit
  end;
  FEstPackage := Model.ModflowPackages.GweEstPackage;


  Abbreviation := 'EST6';

  FPestScriptName  := AFileName;
  GwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := GwtFile;
  FInputFileName := GwtFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, SpeciesIndex);

  FPestParamUsed := False;
  WritingTemplate := False;

  WriteFileInternal;

  if  Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;

end;

procedure TModflowGwtMstWriter.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;
    WriteDataSet0;
    WriteOptions;
    WriteGridData;
  finally
    CloseFile;
  end;
end;

procedure TModflowGwtMstWriter.WriteGridData;
var
  DataArray: TDataArray;
  ChemSpecies: TMobileChemSpeciesItem;
  FileNameToUse: string;
begin
  WriteBeginGridData;
  FileNameToUse := FPestScriptName;

  ChemSpecies := Model.MobileComponents[FSpeciesIndex];

  DataArray := Model.DataArrayManager.GetDataSetByName(
    ChemSpecies.PorosityDataArrayName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'POROSITY');
  WritePestZones(DataArray, FileNameToUse, 'POROSITY', '.' + ChemSpecies.Name, 'POROSITY');

  if FEstPackage.ZeroOrderDecayWater then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.DecayWaterDataArrayName);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'DECAY_WATER');
    WritePestZones(DataArray, FileNameToUse, 'DECAY_WATER', '.' + ChemSpecies.Name, 'DECAY_WATER');
  end;

  if FEstPackage.ZeroOrderDecaySolid then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.DecaySolidDataArrayName);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'DECAY_SOLID');
    WritePestZones(DataArray, FileNameToUse, 'DECAY_SOLID', '.' + ChemSpecies.Name, 'DECAY_SOLID');
  end;

  DataArray := Model.DataArrayManager.GetDataSetByName(
    ChemSpecies.HeatCapacitySolidDataArrayName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'HEAT_CAPACITY_SOLID');
  WritePestZones(DataArray, FileNameToUse, 'HEAT_CAPACITY_SOLID', '.' + ChemSpecies.Name, 'HEAT_CAPACITY_SOLID');

  DataArray := Model.DataArrayManager.GetDataSetByName(
    ChemSpecies.DensitySolidDataArrayName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'DENSITY_SOLID');
  WritePestZones(DataArray, FileNameToUse, 'DENSITY_SOLID', '.' + ChemSpecies.Name, 'DENSITY_SOLID');

  WriteEndGridData;
end;

procedure TModflowGwtMstWriter.WriteOptions;
begin
  WriteBeginOptions;

  WriteSaveFlowsOption;

  if FEstPackage.ZeroOrderDecayWater then
  begin
    WriteString('  ZERO_ORDER_DECAY_WATER');
    NewLine
  end;

  if FEstPackage.ZeroOrderDecaySolid then
  begin
    WriteString('  ZERO_ORDER_DECAY_SOLID');
    NewLine
  end;

  WriteString('  DENSITY_WATER');
  WriteFloat(FEstPackage.DensityWater);
  NewLine;

  WriteString('  HEAT_CAPACITY_WATER');
  WriteFloat(FEstPackage.HeatCapacityWater);
  NewLine;

  WriteString('  LATENT_HEAT_VAPORIZATION');
  WriteFloat(FEstPackage.LatentHeatVaporization);
  NewLine;

  WriteEndOptions;
end;

end.
