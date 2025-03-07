unit ModflowCndWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowCndWriter = class(TCustomPackageWriter)
  private
    FCndPackage: TGweConductionAndDispersionPackage;
    FSpeciesName: string;
    FPestScriptFileName: string;
    procedure WriteFileInternal;
    procedure WriteOptions;
    procedure WriteGridData;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(AFileName: string; SpeciesIndex: Integer);
  end;

implementation

uses
  System.IOUtils, PhastModelUnit, DataSetUnit, DataSetNamesUnit;

{ TModflowCndWriter }

class function TModflowCndWriter.Extension: string;
begin
  result := '.cnd';
end;

function TModflowCndWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GweConductionAndDispersionPackage;
end;

procedure TModflowCndWriter.WriteFile(AFileName: string; SpeciesIndex: Integer);
var
  Abbreviation: string;
  GwtFile: string;
//  SpeciesGwtFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GweUsed then
  begin
    Exit
  end;
  FSpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  FCndPackage := Model.ModflowPackages.GweConductionAndDispersionPackage;

  Abbreviation := 'CND6';
  FPestScriptFileName := AFileName;
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

procedure TModflowCndWriter.WriteFileInternal;
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

procedure TModflowCndWriter.WriteGridData;
var
  DataArray: TDataArray;
  FileNameToUse: string;
begin
  FileNameToUse := FPestScriptFileName;
  WriteBeginGridData;

  DataArray := Model.DataArrayManager.GetDataSetByName(KLongitudinalDispersH + '_' + FSpeciesName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'ALH');
  WritePestZones(DataArray, FileNameToUse, 'ALH', '.' + FSpeciesName, 'ALH');

  DataArray := Model.DataArrayManager.GetDataSetByName(KLongitudinalDispersV + '_' + FSpeciesName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'ALV');
  WritePestZones(DataArray, FileNameToUse, 'ALV', '.' + FSpeciesName, 'ALV');

  DataArray := Model.DataArrayManager.GetDataSetByName(KHorizontalTransvers + '_' + FSpeciesName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'ATH1');
  WritePestZones(DataArray, FileNameToUse, 'ATH1', '.' + FSpeciesName, 'ATH1');

  DataArray := Model.DataArrayManager.GetDataSetByName(KVerticalTransverse + '_' + FSpeciesName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'ATH2');
  WritePestZones(DataArray, FileNameToUse, 'ATH2', '.' + FSpeciesName, 'ATH2');

  DataArray := Model.DataArrayManager.GetDataSetByName(rsVertical_Transv_Dispersivity + '_' + FSpeciesName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'ATV');
  WritePestZones(DataArray, FileNameToUse, 'ATV', '.' + FSpeciesName, 'ATV');

  DataArray := Model.DataArrayManager.GetDataSetByName(rsThermalCondFluid + '_' + FSpeciesName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'KTW');
  WritePestZones(DataArray, FileNameToUse, 'KTW', '.' + FSpeciesName, 'KTW');

  DataArray := Model.DataArrayManager.GetDataSetByName(rsThermalCondSolid + '_' + FSpeciesName);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'KTS');
  WritePestZones(DataArray, FileNameToUse, 'KTS', '.' + FSpeciesName, 'KTS');

  WriteEndGridData;
end;

procedure TModflowCndWriter.WriteOptions;
begin
  WriteBeginOptions;
  try
    if not FCndPackage.UseXt3d then
    begin
      WriteString('  XT3D_OFF');
      NewLine;
    end
    else
    begin
      if FCndPackage.Xt3dRightHandSide then
      begin
        WriteString('  XT3D_RHS');
        NewLine;
      end;
    end;
    WriteExportAsciiArray;
  finally
    WriteEndOptions;
  end;
end;

end.
