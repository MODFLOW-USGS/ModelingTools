unit ModflowDspWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowDspWriter = class(TCustomPackageWriter)
  private
    FDspPackage: TGwtDispersionPackage;
    FSpeciesName: string;
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

{ TModflowDspWriter }

class function TModflowDspWriter.Extension: string;
begin
  result := '.dsp';
end;

function TModflowDspWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GwtDispersionPackage;
end;

procedure TModflowDspWriter.WriteFile(AFileName: string; SpeciesIndex: Integer);
var
  Abbreviation: string;
  GwtFile: string;
//  SpeciesGwtFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GwtUsed then
  begin
    Exit
  end;
  FSpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  FDspPackage := Model.ModflowPackages.GwtDispersionPackage;

  Abbreviation := 'DSP6';
  GwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := GwtFile;

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

//  SpeciesGwtFile := GwtFileName(AFileName, SpeciesIndex);
//  WriteToGwtNameFile(Abbreviation, SpeciesGwtFile, SpeciesIndex);
//  TFile.Copy(GwtFile, SpeciesGwtFile, True);
//  if  Model.PestUsed and FPestParamUsed then
//  begin
//    TFile.Copy(GwtFile + '.tpl', SpeciesGwtFile + '.tpl', True);
//  end;
end;

procedure TModflowDspWriter.WriteFileInternal;
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

procedure TModflowDspWriter.WriteGridData;
var
  DataArray: TDataArray;
begin
  WriteBeginGridData;

  if FDspPackage.SeparateDataSetsForEachSpecies = dtCombined then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KDiffusionCoefficien);
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KDiffusionCoefficien + '_' + FSpeciesName);
  end;
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'DIFFC');

  case FDspPackage.LongitudinalDispTreatement of
    dtCombined:
      begin
        if FDspPackage.SeparateDataSetsForEachSpecies = dtCombined then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(rsLong_Dispersivity);
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KLongitudinalDispersH + '_' + FSpeciesName);
        end;
        Assert(DataArray <> nil);
        WriteMf6_DataSet(DataArray, 'ALH');
      end;
    dtSeparate:
      begin
        if FDspPackage.SeparateDataSetsForEachSpecies = dtCombined then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KLongitudinalDispersH);
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KLongitudinalDispersH + '_' + FSpeciesName);
        end;
        Assert(DataArray <> nil);
        WriteMf6_DataSet(DataArray, 'ALH');

        if FDspPackage.SeparateDataSetsForEachSpecies = dtCombined then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KLongitudinalDispersV);
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KLongitudinalDispersV + '_' + FSpeciesName);
        end;
        Assert(DataArray <> nil);
        WriteMf6_DataSet(DataArray, 'ALV');
      end;
    else
      Assert(False);
  end;

  case FDspPackage.TransverseDispTreatement of
    dtCombined:
      begin
        if FDspPackage.SeparateDataSetsForEachSpecies = dtCombined then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(rsHorizontal_Transv_Dispersivity);
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KHorizontalTransvers + '_' + FSpeciesName);
        end;
        Assert(DataArray <> nil);
        WriteMf6_DataSet(DataArray, 'ATH1');
      end;
    dtSeparate:
      begin
        if FDspPackage.SeparateDataSetsForEachSpecies = dtCombined then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KHorizontalTransvers);
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KHorizontalTransvers + '_' + FSpeciesName);
        end;
        Assert(DataArray <> nil);
        WriteMf6_DataSet(DataArray, 'ATH1');

        if  FDspPackage.SeparateDataSetsForEachSpecies = dtCombined then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KVerticalTransverse);
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(KVerticalTransverse + '_' + FSpeciesName);
        end;
        Assert(DataArray <> nil);
        WriteMf6_DataSet(DataArray, 'ATH2');
      end;
    else
      Assert(False);
  end;

  if FDspPackage.UseTransverseDispForVertFlow then
  begin
    if FDspPackage.SeparateDataSetsForEachSpecies = dtCombined then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(rsVertical_Transv_Dispersivity);
    end
    else
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(rsVertical_Transv_Dispersivity + '_' + FSpeciesName);
    end;
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'ATV');
  end;

  WriteEndGridData;
end;

procedure TModflowDspWriter.WriteOptions;
begin
  WriteBeginOptions;
  if not FDspPackage.UseXt3d then
  begin
    WriteString('  XT3D_OFF');
    NewLine;
  end
  else
  begin
    if FDspPackage.Xt3dRightHandSide then
    begin
    WriteString('  XT3D_RHS');
    NewLine;
    end;
  end;
  WriteEndOptions;
end;

end.
