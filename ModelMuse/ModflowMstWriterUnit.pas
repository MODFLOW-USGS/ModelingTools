unit ModflowMstWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowGwtMstWriter = class(TCustomPackageWriter)
  private
    FSpeciesIndex: Integer;
    FMstPackage: TGwtMstPackage;
    FPestScriptName: string;
    procedure WriteOptions;
    procedure writeGridData;
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
  Result := '.mst';
end;

function TModflowGwtMstWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GwtPackages[FSpeciesIndex].GwtMst;
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
  if not Model.GwtUsed then
  begin
    Exit
  end;
  FMstPackage := Model.ModflowPackages.GwtPackages[FSpeciesIndex].GwtMst;


  Abbreviation := 'MST6';

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
    writeGridData;
  finally
    CloseFile;
  end;
end;

procedure TModflowGwtMstWriter.writeGridData;
var
  DataArray: TDataArray;
  ChemSpecies: TMobileChemSpeciesItem;
  FileNameToUse: string;
begin
  WriteBeginGridData;
  FileNameToUse := FPestScriptName;

  ChemSpecies := Model.MobileComponents[FSpeciesIndex];

  if FMstPackage.SeparatePorosity then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.PorosityDataArrayName);
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsPorosity);
  end;
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'POROSITY');
  WritePestZones(DataArray, FileNameToUse, 'POROSITY', '.' + ChemSpecies.Name, 'POROSITY');

  if FMstPackage.ZeroOrderDecay or FMstPackage.FirstOrderDecay then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.MobileDecayRateDataArrayName);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'DECAY');
    WritePestZones(DataArray, FileNameToUse, 'DECAY', '.' + ChemSpecies.Name, 'DECAY');

    if FMstPackage.Sorption <> gscNone then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        ChemSpecies.MobileSorbedDecayRateDataArrayName);
      Assert(DataArray <> nil);
      WriteMf6_DataSet(DataArray, 'DECAY_SORBED');
      WritePestZones(DataArray, FileNameToUse, 'DECAY_SORBED', '.' + ChemSpecies.Name, 'DECAY_SORBED');
    end;
  end;

  if FMstPackage.Sorption <> gscNone then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.MobileBulkDensityDataArrayName);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'BULK_DENSITY');
    WritePestZones(DataArray, FileNameToUse, 'BULK_DENSITY', '.' + ChemSpecies.Name, 'BULK_DENSITY');

    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.MobileDistCoefDataArrayName);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'DISTCOEF');
    WritePestZones(DataArray, FileNameToUse, 'DISTCOEF', '.' + ChemSpecies.Name, 'DISTCOEF');
  end;

  if FMstPackage.Sorption = gscFreundlich then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.MobileFreundlichExponentDataArrayName);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'SP2');
    WritePestZones(DataArray, FileNameToUse, 'SP2', '.' + ChemSpecies.Name, 'SP2');
  end;

  if FMstPackage.Sorption = gscLangmuir then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.MobileSorptionCapacityDataArrayName);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'SP2');
    WritePestZones(DataArray, FileNameToUse, 'SP2', '.' + ChemSpecies.Name, 'SP2');
  end;

  WriteEndGridData;
end;

procedure TModflowGwtMstWriter.WriteOptions;
begin
  WriteBeginOptions;

  WriteSaveFlowsOption;

  if FMstPackage.ZeroOrderDecay then
  begin
    WriteString('  ZERO_ORDER_DECAY');
    NewLine
  end;

  if FMstPackage.FirstOrderDecay then
  begin
    WriteString('  FIRST_ORDER_DECAY');
    NewLine
  end;

  case FMstPackage.Sorption of
    gscNone:
      begin
        // do nothing
      end;
    gscLinear:
      begin
        WriteString('  SORPTION LINEAR');
        NewLine
      end;
    gscFreundlich:
      begin
        WriteString('  SORPTION FREUNDLICH');
        NewLine
      end;
    gscLangmuir:
      begin
        WriteString('  SORPTION LANGMUIR');
        NewLine
      end;
  end;

  WriteEndOptions;
end;

end.
