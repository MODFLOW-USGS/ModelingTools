unit ModflowIstWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, System.SysUtils;

type
  TModflowGwtIstWriter = class(TCustomPackageWriter)
  private
    FSpeciesIndex: Integer;
    FIstPackage: TGwtIstPackage;
    FIstProp: TIstPackageItem;
    FDomainIndex: Integer;
    FGwtFile: string;
    function IstFileName(const AFileName: string; SpeciesIndex,
      PackageIndex: Integer): string;
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
  DataSetUnit, Mt3dmsChemSpeciesUnit, PhastModelUnit;
{ TModflowGwtIstWriter }

class function TModflowGwtIstWriter.Extension: string;
begin
  result := '.ist';
end;

function TModflowGwtIstWriter.IstFileName(const AFileName: string; SpeciesIndex,
  PackageIndex: Integer): string;
var
  PackageId: string;
begin

  PackageId := '.' + Model.MobileComponents[SpeciesIndex].Name
    + '.' + IntToStr(PackageIndex);
  result := ChangeFileExt(AFileName, PackageId) + Extension;
end;

function TModflowGwtIstWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GwtPackages[FSpeciesIndex].GwtIst;
end;

procedure TModflowGwtIstWriter.WriteFile(AFileName: string;
  SpeciesIndex: Integer);
var
  Abbreviation: string;
  DomainIndex: Integer;
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
  FIstPackage := Model.ModflowPackages.GwtPackages[FSpeciesIndex].GwtIst;

  Abbreviation := 'IST6';

  for DomainIndex := 0 to FIstPackage.IstPackageProperties.Count - 1 do
  begin
    FIstProp := FIstPackage.IstPackageProperties[DomainIndex];
    FDomainIndex := DomainIndex;
    FGwtFile := IstFileName(AFileName, SpeciesIndex, DomainIndex+1);
    FNameOfFile := FGwtFile;

    WriteToGwtNameFile(Abbreviation, FNameOfFile, SpeciesIndex,
      'IST-' + IntToStr(DomainIndex+1));

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

end;

procedure TModflowGwtIstWriter.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
    writeGridData;
  finally
    CloseFile;
  end;
end;

procedure TModflowGwtIstWriter.writeGridData;
var
  DataArray: TDataArray;
  ChemSpecies: TMobileChemSpeciesItem;
begin
  WriteBeginGridData;

  ChemSpecies := Model.MobileComponents[FSpeciesIndex];

  if FDomainIndex < ChemSpecies.ImmobileInitialConcentrations.Count then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.ImmobileInitialConcentrations[FDomainIndex]);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'CIM');
  end;

  if FDomainIndex < ChemSpecies.ImmobilePorosities.Count then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.ImmobilePorosities[FDomainIndex]);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'THETAIM');
  end;

  if FDomainIndex < ChemSpecies.ImmobileMassTransferRates.Count then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.ImmobileMassTransferRates[FDomainIndex]);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'ZETAIM');
  end;

  if (FIstProp.FirstOrderDecay or FIstProp.ZeroOrderDecay)
    and (FDomainIndex < ChemSpecies.ImmobileDecay.Count) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.ImmobileDecay[FDomainIndex]);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'DECAY');
  end;

  if FIstProp.Sorption and (FIstProp.FirstOrderDecay or FIstProp.ZeroOrderDecay)
    and (FDomainIndex < ChemSpecies.ImmobileDecaySorbed.Count) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.ImmobileDecaySorbed[FDomainIndex]);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'DECAY_SORBED');
  end;

  if FIstProp.Sorption
    and (FDomainIndex < ChemSpecies.ImmobileBulkDensities.Count) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.ImmobileBulkDensities[FDomainIndex]);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'BULK_DENSITY');
  end;

  if FIstProp.Sorption
    and (FDomainIndex < ChemSpecies.ImmobileDistCoeficients.Count) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      ChemSpecies.ImmobileDistCoeficients[FDomainIndex]);
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'DISTCOEF');
  end;

  WriteEndGridData;
end;

procedure TModflowGwtIstWriter.WriteOptions;
var
  Budgetfile: string;
  Cimfile: string;
begin
  WriteBeginOptions;

  WriteSaveFlowsOption;

  if FIstProp.BinaryBudgetFileOut then
  begin
    WriteString('  BUDGET FILEOUT ');
    Budgetfile := ChangeFileExt(FGwtFile, '.budget');
    Model.AddModelOutputFile(Budgetfile);
    Budgetfile := ExtractFileName(Budgetfile);
    WriteString(Budgetfile);
    NewLine;
  end;

  if FIstProp.TextBudgetFileOut then
  begin
    WriteString('  BUDGETCSV FILEOUT ');
    Budgetfile := ChangeFileExt(FGwtFile, '.budget.csv');
    Model.AddModelOutputFile(Budgetfile);
    Budgetfile := ExtractFileName(Budgetfile);
    WriteString(Budgetfile);
    NewLine;
  end;

  if FIstProp.Sorption then
  begin
    WriteString('  SORPTION');
    NewLine
  end;

  if FIstProp.ZeroOrderDecay then
  begin
    WriteString('  ZERO_ORDER_DECAY');
    NewLine
  end;

  if FIstProp.FirstOrderDecay then
  begin
    WriteString('  FIRST_ORDER_DECAY');
    NewLine
  end;

  if FIstProp.SaveConcentrations then
  begin
    WriteString('  CIM FILEOUT ');
    Cimfile := ChangeFileExt(FGwtFile, '.cim');
    Model.AddModelOutputFile(Cimfile);
    Cimfile := ExtractFileName(Cimfile);
    WriteString(Cimfile);
    NewLine;
  end;

  if FIstProp.SpecifyPrintFormat then
  begin
    WriteString('  CIM PRINT_FORMAT COLUMNS');
    WriteInteger(FIstProp.Columns);
    WriteString(' WIDTH');
    WriteInteger(FIstProp.Width);
    WriteString(' DIGITS');
    WriteInteger(FIstProp.Digits);
    case FIstProp.PrintFormat of
      pfExponential:
        begin
          WriteString(' EXPONENTIAL')
        end;
      pfFixed:
        begin
          WriteString(' FIXED')
        end;
      pfGeneral:
        begin
          WriteString(' GENERAL')
        end;
      pfScientific:
        begin
          WriteString(' SCIENTIFIC')
        end;
      else
        Assert(False);
    end;
    NewLine;
  end;

  WriteEndOptions;
end;

end.
