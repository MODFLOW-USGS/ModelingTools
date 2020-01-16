unit ModflowCSubWriterUnit;

interface

uses
  System.SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit,
  PhastModelUnit, SparseDataSets, DataSetUnit, ModflowCSubInterbed;

type
  TCSubWriter = class(TCustomTransientWriter)
  private
    FCSubPackage: TCSubPackageSelection;
    FFileName: string;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WriteGridData;
    procedure WritePackageData;
    procedure WriteStressPeriods;
  protected
    function Package: TModflowPackageSelection; override;
    procedure Evaluate; override;
    class function Extension: string; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmProgressUnit;

{ TCSubWriter }

constructor TCSubWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FCSubPackage := Package as TCSubPackageSelection;
end;

procedure TCSubWriter.Evaluate;
begin
  inherited;

end;

class function TCSubWriter.Extension: string;
begin
  result := '.csub';
end;

function TCSubWriter.Package: TModflowPackageSelection;
begin

end;

procedure TCSubWriter.WriteDimensions;
begin
  WriteBeginDimensions;

  WriteEndDimensions
end;

procedure TCSubWriter.WriteFile(const AFileName: string);
begin
  FFileName := FileName(AFileName);
end;

procedure TCSubWriter.WriteGridData;
var
  DataArray: TDataArray;
begin
  WriteBeginGridData;

  frmProgressMM.AddMessage('  Writing CG_SKE_CR');
  if FCSubPackage.CompressionMethod = coRecompression then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KInitialElasticReco);
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KInitialElasticSpec);
  end;
  WriteMf6_DataSet(DataArray, 'CG_SKE_CR');

  frmProgressMM.AddMessage('  Writing CG_THETA');
  DataArray := Model.DataArrayManager.GetDataSetByName(KInitialCoarsePoros);
  WriteMf6_DataSet(DataArray, 'CG_THETA');

  frmProgressMM.AddMessage('  Writing SGM');
  DataArray := Model.DataArrayManager.GetDataSetByName(KMoistSpecificGravi);
  WriteMf6_DataSet(DataArray, 'SGM');

  frmProgressMM.AddMessage('  Writing SGS');
  DataArray := Model.DataArrayManager.GetDataSetByName(KSaturatedSpecificG);
  WriteMf6_DataSet(DataArray, 'SGS');

  WriteEndGridData;
end;

procedure TCSubWriter.WriteOptions;
var
  OutputFileName: string;
begin
  WriteBeginOptions;

//  [BOUNDNAMES]
//  [PRINT_INPUT]
//  [SAVE_FLOWS]

//  [GAMMAW <gammaw>]
  WriteString('  GAMMAW ');
  WriteFloat(FCSubPackage.Gamma);
  NewLine;

//  [BETA <beta>]
  WriteString('  BETA ');
  WriteFloat(FCSubPackage.Beta);
  NewLine;

//  [HEAD_BASED]
  if FCSubPackage.HeadBased then
  begin
    WriteString('  HEAD_BASED');
    NewLine;
  end;

//  [INITIAL_PRECONSOLIDATION_HEAD]
  if FCSubPackage.PreconsolidationHeadUsed then
  begin
    WriteString('  INITIAL_PRECONSOLIDATION_HEAD');
    NewLine;
  end;

//  [NDELAYCELLS <ndelaycells>]
  WriteString('  NDELAYCELLS ');
  WriteInteger(FCSubPackage.NumberOfDelayCells);
  NewLine;

//  [COMPRESSION_INDICES]
  if FCSubPackage.CompressionMethod = coRecompression then
  begin
    WriteString('  COMPRESSION_INDICES');
    NewLine;
  end;

//  [UPDATE_MATERIAL_PROPERTIES]
  if FCSubPackage.UpdateMaterialProperties then
  begin
    WriteString('  UPDATE_MATERIAL_PROPERTIES');
    NewLine;
  end;

//  [CELL_FRACTION]
  if FCSubPackage.InterbedThicknessMethod = itmCellFraction then
  begin
    WriteString('  CELL_FRACTION');
    NewLine;
  end;

//  [SPECIFIED_INITIAL_INTERBED_STATE]

//  [SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS]
  if FCSubPackage.SpecifyInitialPreconsolidationStress then
  begin
    WriteString('  SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS');
    NewLine;
  end;

//  [SPECIFIED_INITIAL_DELAY_HEAD]
  if FCSubPackage.SpecifyInitialDelayHead then
  begin
    WriteString('  SPECIFIED_INITIAL_DELAY_HEAD');
    NewLine;
  end;

//  [EFFECTIVE_STRESS_LAG]
  if FCSubPackage.EffectiveStressLag then
  begin
    WriteString('  EFFECTIVE_STRESS_LAG');
    NewLine;
  end;

//  [STRAIN_CSV_INTERBED FILEOUT <interbedstrain_filename>]
  if coInterbedStrain in FCSubPackage.OutputTypes then
  begin
    WriteString('  STRAIN_CSV_INTERBED FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, '.csub_strn');
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [STRAIN_CSV_COARSE FILEOUT <coarsestrain_filename>]
  if coCourseStrain in FCSubPackage.OutputTypes then
  begin
    WriteString('  STRAIN_CSV_COARSE FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, '.csub_crs_strn');
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION FILEOUT <compaction_filename>]
  if coCompaction in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, '.csub_cmpct');
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION_ELASTIC FILEOUT <elastic_compaction_filename>]
  if coElasticComp in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION_ELASTIC FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, '.csub_elst_cmpct');
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION_INELASTIC FILEOUT <inelastic_compaction_filename>]
  if coInelasticComp in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION_INELASTIC FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, '.csub_inelst_cmpct');
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION_INTERBED FILEOUT <interbed_compaction_filename>]
  if coInterbedComp in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION_INTERBED FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, '.csub_intrbd_cmpct');
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION_COARSE FILEOUT <coarse_compaction_filename>]
  if coCoarseComp in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION_COARSE FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, '.csub_crs_cmpct');
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [ZDISPLACEMENT FILEOUT <zdisplacement_filename>]
  if coZDisplacement in FCSubPackage.OutputTypes then
  begin
    WriteString('  ZDISPLACEMENT FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, '.csub_z_dis');
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [TS6 FILEIN <ts6_filename>]
//  [OBS6 FILEIN <obs6_filename>]

  WriteEndOptions
end;

procedure TCSubWriter.WritePackageData;
begin
  WriteBeginPackageData;

  WriteEndPackageData
end;

procedure TCSubWriter.WriteStressPeriods;
begin

end;

end.
