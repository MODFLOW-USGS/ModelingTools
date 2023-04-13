unit ModflowCSubWriterUnit;

interface

uses
  System.SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit,
  PhastModelUnit, DataSetUnit, ModflowCSubInterbed,
  System.Classes, ModflowCellUnit, ModflowCsubUnit, GoPhastTypes,
  System.Generics.Collections, ScreenObjectUnit, ModflowBoundaryDisplayUnit,
  Modflow6ObsUnit, DataArrayManagerUnit;

type
  TCSubObservation = record
    FName: string;
    FBoundName: string;
    FObsTypes: TSubObsSet;
    FScreenObject: TObject;
    FInterbedNumbers: TOneDIntegerArray;
    FDelayInterbeds: TBoolArray;
    FDelayCellNumbers: TOneDIntegerArray;
    FCells: array of TCellLocation;
    FModflow6Obs: TModflow6Obs;
  end;
  TCSubObservationList = TList<TCSubObservation>;

  TCSubWriter = class(TCustomTransientWriter)
  private
    // After @link(Evaluate) is called,
    // @name contains a series of @link(TValueCellList)s;
    // one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for one stress period.
    // @name is a TObjectList.
//    FValues: TList;
    FCSubPackage: TCSubPackageSelection;
    FFileName: string;
    FStressPeriod: Integer;
    FBoundaryIndex: Integer;
    FObservations: TCSubObservationList;
    FInterBedNumbers: array of array of array of array of Integer;
    ninterbeds: Integer;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WriteGridData;
    procedure WritePackageData;
    procedure WriteStressPeriods;
    function ObservationsUsed: Boolean;  reintroduce;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; reintroduce;
    procedure WriteFileInternal;
  protected
    function Package: TModflowPackageSelection; override;
    procedure Evaluate; override;
    procedure WriteAndCheckCells(List: TValueCellList;
      TimeIndex: integer);
    procedure WriteCell(Cell: TValueCell);
    class function ObservationExtension: string; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    class function Extension: string; override;
  end;

implementation

uses
  frmProgressUnit, frmErrorsAndWarningsUnit,
  Vcl.Forms, Modflow6ObsWriterUnit, PestParamRoots, DataSetNamesUnit;

resourcestring
  StrNoTransientCSUBDa = 'No Transient CSUB data defined';
  StrNoTransientDataIs = 'No transient data is defined for the CSUB package.' +
  ' The CSUB package does not require transient data.';


{ TCSubWriter }

constructor TCSubWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FCSubPackage := Package as TCSubPackageSelection;
  FObservations := TCSubObservationList.Create;
//  FValues := TObjectList.Create;
end;

destructor TCSubWriter.Destroy;
begin
  FObservations.Free;
//  FValues.Free;
  inherited;
end;

procedure TCSubWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  NoAssignmentErrorRoot: string;
  Boundary: TCSubBoundary;
  MfObs: TModflow6Obs;
  Obs: TCSubObservation;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
  IDomainArray: TDataArray;
  CellCount: Integer;
  IbIndex: Integer;
begin
  NoAssignmentErrorRoot := Format(StrNoBoundaryConditio,
    [Package.PackageIdentifier]);
  frmErrorsAndWarnings.BeginUpdate;
  IDomainArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  IDomainArray.Initialize;
  CellList := TCellAssignmentList.Create;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoAssignmentErrorRoot);
    frmProgressMM.AddMessage('Evaluating CSUB Package data.');

    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowCSub;
      if Boundary <> nil then
      begin
        if ObservationsUsed and IsMf6Observation(ScreenObject) then
        begin
          MfObs := ScreenObject.Modflow6Obs;
          Obs.FName := MfObs.Name;
          Obs.FBoundName := ScreenObject.Name;
          Obs.FObsTypes := MfObs.CSubObs.CSubObsSet;
          Obs.FScreenObject := ScreenObject;
          Obs.FModflow6Obs := MfObs;
          CellList.Clear;
          ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          SetLength(Obs.FCells, CellList.Count);
          CellCount := 0;
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            if IDomainArray.IntegerData[ACell.Layer, ACell.Row, ACell.Column] > 0 then
            begin
              Obs.FCells[CellCount] := ACell.Cell;
              Inc(CellCount);
            end;
          end;
          SetLength(Obs.FCells, CellCount);
          SetLength(Obs.FDelayCellNumbers, MfObs.CSubDelayCells.Count);
          for IbIndex := 0 to MfObs.CSubDelayCells.Count - 1 do
          begin
            Obs.FDelayCellNumbers[IbIndex] := MfObs.CSubDelayCells[IbIndex].Value;
          end;
          FObservations.Add(Obs);
        end;
        frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
        if not ScreenObject.SetValuesOfEnclosedCells
          and not ScreenObject.SetValuesOfIntersectedCells then
        begin
          frmErrorsAndWarnings.AddError(Model,
            NoAssignmentErrorRoot, ScreenObject.Name, ScreenObject);
        end;
        Boundary.GetCellValues(Values, nil, Model, self);
        FTimeSeriesNames.AddStrings(Boundary.Mf6TimeSeriesNames);
      end
      else if ObservationsUsed and IsMf6Observation(ScreenObject) then
      begin
        MfObs := ScreenObject.Modflow6Obs;
        Obs.FName := MfObs.Name;
        Obs.FBoundName := ScreenObject.Name;
        Obs.FObsTypes := MfObs.CSubObs.CSubObsSet;
        Obs.FScreenObject := ScreenObject;
        Obs.FModflow6Obs := MfObs;
        CellList.Clear;
        ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        SetLength(Obs.FCells, CellList.Count);
        CellCount := 0;
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          if IDomainArray.IntegerData[ACell.Layer, ACell.Row, ACell.Column] > 0 then
          begin
            Obs.FCells[CellCount] := ACell.Cell;
            Inc(CellCount);
          end;
        end;
        SetLength(Obs.FCells, CellCount);
        SetLength(Obs.FDelayCellNumbers, MfObs.CSubDelayCells.Count);
        for IbIndex := 0 to MfObs.CSubDelayCells.Count - 1 do
        begin
          Obs.FDelayCellNumbers[IbIndex] := MfObs.CSubDelayCells[IbIndex].Value;
        end;

        FObservations.Add(Obs);
      end;
    end;
  finally
    CellList.fREE;;
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TCSubWriter.Extension: string;
begin
  result := '.csub';
end;

function TCSubWriter.IsMf6Observation(AScreenObject: TScreenObject): Boolean;
var
  MfObs: TModflow6Obs;
begin
  MfObs := AScreenObject.Modflow6Obs;
  Result := (MfObs <> nil) and MfObs.Used and (MfObs.CSubObs.CSubObsSet <> []);
end;

class function TCSubWriter.ObservationExtension: string;
begin
  result := '.ob_csub';
end;

function TCSubWriter.ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TCSubWriter.Package: TModflowPackageSelection;
begin
  Result := Model.ModflowPackages.CsubPackage;
end;

procedure TCSubWriter.UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
var
  DataArrayList: TList;
  TimeListIndex: Integer;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  TimeIndex: Integer;
  CellList: TValueCellList;
  DataArray: TModflowBoundaryDisplayDataArray;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TCSubBoundary;
begin
  // Quit if the package isn't used.
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    DataArrayList := TList.Create;
    try
      // evaluate all the data used in the package.
      Evaluate;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;

      end;
      for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
      begin
        ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        if not ScreenObject.UsedModels.UsesModel(Model) then
        begin
          Continue;
        end;
        Boundary := ScreenObject.ModflowCSub;
        if Boundary <> nil then
        begin
          Boundary.ClearTimeLists(Model);
        end;
      end;

      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        DisplayTimeList := TimeLists[TimeListIndex];
        // Values.Count can be zero if no objects define the boundary condition.
        if (Values.Count <> 0) or (DisplayTimeList.Count = 0) then
        begin
          Assert(Values.Count = DisplayTimeList.Count);
        end;
      end;

      // For each stress period, transfer values from
      // the cells lists to the data arrays.
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        CellList := Values[TimeIndex];
        if CellList.Count > 0 then
        begin
          DataArrayList.Clear;
          for TimeListIndex := 0 to TimeLists.Count - 1 do
          begin
            DisplayTimeList := TimeLists[TimeListIndex];
            DataArray := DisplayTimeList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArrayList.Add(DataArray);
          end;
          UpdateCellDisplay(CellList, DataArrayList, []);
        end;
      end;

      // Mark all the data arrays and time lists as up to date.
      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        DisplayTimeList := TimeLists[TimeListIndex];
        for TimeIndex := 0 to DisplayTimeList.Count - 1 do
        begin
          DataArray := DisplayTimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.UpToDate := True;
        end;
        DisplayTimeList.SetUpToDate(True);
      end;

      
      
    finally
      DataArrayList.Free;
    end;
  
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;

end;

procedure TCSubWriter.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;

    frmProgressMM.AddMessage('Writing CSUB Package input.');

    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingOptions);
    WriteOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDimensions);
    WriteDimensions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if MAXBOUND = 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrNoTransientCSUBDa, StrNoTransientDataIs);
    end;

    frmProgressMM.AddMessage(StrWritingGridData);
    WriteGridData;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingPackageData);
    WritePackageData;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingStressPerio);
    WriteStressPeriods;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

procedure TCSubWriter.WriteAndCheckCells(List: TValueCellList;
  TimeIndex: integer);
var
  CellIndex: Integer;
  Cell: TValueCell;
  ShouldWrite: Boolean;
  ActiveDS: TDataArray;
begin
 ActiveDS := Model.DataArrayManager.GetDataSetByName(rsActive);
  for CellIndex := 0 to List.Count - 1 do
  begin
    Cell := List[CellIndex] as TValueCell;
    ShouldWrite := ActiveDS.BooleanData[Cell.Layer,
      Cell.Row, Cell.Column];
    if ShouldWrite then
    begin
      WriteCell(Cell);
    end;
    CheckCell(Cell, Package.PackageIdentifier);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  end;
end;

procedure TCSubWriter.WriteCell(Cell: TValueCell);
const
  DataSetIdentifier = '';
  VariableIdentifiers = 'sig0';
var
  CSubCell: TCSubCell;
  LocalLayer: integer;
begin
  Inc(FBoundaryIndex);

  CSubCell := Cell as TCSubCell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(CSubCell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(CSubCell.Row+1);
  end;
  WriteInteger(CSubCell.Column+1);
  WriteValueOrFormula(CSubCell, CsubStressOffsetPosition);
//  WriteFloat(CSubCell.StressOffset);
  WriteBoundName(CSubCell);
  if Model.DisvUsed then
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer cell2d '
      + VariableIdentifiers + ' ');
  end
  else
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer Row Column '
      + VariableIdentifiers + ' ');
  end;
  // The annotation identifies the object used to define the well.
  // This can be helpful in identifying when used with PEST.
  WriteString(CSubCell.StressOffsetAnnotation);
  NewLine;
end;

procedure TCSubWriter.WriteDimensions;
var
  DataArrayManager: TDataArrayManager;
  InterbedIndex: Integer;
  Interbed: TCSubInterbed;
  pcsDataArray: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  IDomain: TDataArray;
begin
  WriteBeginDimensions;

  ninterbeds := 0;
  DataArrayManager := Model.DataArrayManager;
  IDomain := DataArrayManager.GetDataSetByName(K_IDOMAIN);

  for InterbedIndex := 0 to FCSubPackage.Interbeds.Count - 1 do
  begin
    Interbed := FCSubPackage.Interbeds[InterbedIndex];
    pcsDataArray := DataArrayManager.GetDataSetByName(Interbed.InitialOffset);
    pcsDataArray.Initialize;
    for LayerIndex := 0 to Model.LayerCount - 1 do
    begin
      for RowIndex := 0 to Model.RowCount - 1 do
      begin
        for ColIndex := 0 to Model.ColumnCount - 1 do
        begin
          if pcsDataArray.IsValue[LayerIndex, RowIndex, ColIndex]
            and (IDomain.IntegerData[LayerIndex, RowIndex, ColIndex] > 0) then
          begin
            Inc(ninterbeds);
          end;
        end;
      end;
    end;
  end;
  
  WriteString('  NINTERBEDS');
  WriteInteger(ninterbeds);
  NewLine;

  CountCells(MAXBOUND);
  WriteString('  MAXSIG0');
  WriteInteger(MAXBOUND);
  NewLine;


  WriteEndDimensions
end;

procedure TCSubWriter.WriteFile(const AFileName: string);
var
  Abbreviation: string;
  ObsWriter: TCSubObsWriter;
  ObsIndex: Integer;
  CSubObs: TCSubObservation;
  IbObsTypes: TSubObsSet;
  CellIndex: Integer;
  InterbedNumbers: TOneDIntegerArray;
  DelayInterbeds: TBoolArray;
  IBCount: Integer;
  ACell: TCellLocation;
  InterbedSystemCount: Integer;
  IbIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  Abbreviation := 'CSUB6';
  if Model.PackageGeneratedExternally(Abbreviation) then
  begin
    Exit;
  end;

  FFileName := FileName(AFileName);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

//  ClearTimeLists(Model);
  FInputFileName := FFileName;
  WriteToNameFile(Abbreviation, 0, FFileName, foInput, Model, False, 'CSUB');
  FNameOfFile := FFileName;
  WriteFileInternal;

  if FObservations.Count > 0 then
  begin
    InterbedSystemCount := FCSubPackage.Interbeds.Count;
    for ObsIndex := 0 to FObservations.Count - 1 do
    begin
      CSubObs := FObservations[ObsIndex];
      IbObsTypes := [coCSub, coInelastCSub, coElastCSub, coSk, coSke, coIntbedComp,
            coInelastComp, coElastComp, coThickness, coTheta, coDelayFlowTop, coDelayFlowBot, coDelayHead,
        coDelayGStress, coDelayEStress, coDelayPreConStress, coDelayComp,
        coDelayThickness, coDelayTheta]
        * CSubObs.FObsTypes;
      if IbObsTypes <> [] then
      begin
        SetLength(InterbedNumbers, Length(CSubObs.FCells) * InterbedSystemCount);
        SetLength(DelayInterbeds, Length(CSubObs.FCells) * InterbedSystemCount);
        IBCount := 0;
        for CellIndex := 0 to Length(CSubObs.FCells) - 1 do
        begin
          ACell := CSubObs.FCells[CellIndex];
          // FInterBedNumbers is specified in WritePackageData.
          if FInterBedNumbers[ACell.Layer, ACell.Row, ACell.Column] <> nil then
          begin
            for IbIndex := 0 to InterbedSystemCount -1 do
            begin
              if FInterBedNumbers[ACell.Layer, ACell.Row, ACell.Column, IbIndex] <> 0 then
              begin
                InterbedNumbers[IBCount] :=
                  FInterBedNumbers[ACell.Layer, ACell.Row, ACell.Column, IbIndex];
                DelayInterbeds[IBCount] :=
                  FCSubPackage.Interbeds[IbIndex].InterbedType = itDelay;
                Inc(IBCount);
              end;
            end;
          end;
        end;
        SetLength(InterbedNumbers, IBCount);
        CSubObs.FInterbedNumbers := InterbedNumbers;
        SetLength(DelayInterbeds, IBCount);
        CSubObs.FDelayInterbeds := DelayInterbeds;
        FObservations[ObsIndex] := CSubObs;
      end;
    end;

    ObsWriter := TCSubObsWriter.Create(Model, etExport, FObservations);
    try
      ObsWriter.WriteFile(ChangeFileExt(FFileName, ObservationExtension));
    finally
      ObsWriter.Free;
    end;
  end;

  if  Model.PestUsed and FPestParamUsed then
  begin
    frmErrorsAndWarnings.BeginUpdate;
    try
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;

    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  end;

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
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'CG_SKE_CR');
  WritePestZones(DataArray, FInputFileName, CSUB_CG_SKE_CR, 'CSC');

  frmProgressMM.AddMessage('  Writing CG_THETA');
  DataArray := Model.DataArrayManager.GetDataSetByName(KInitialCoarsePoros);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'CG_THETA');
  WritePestZones(DataArray, FInputFileName, CSUB_CG_THETA, 'CCT');

  frmProgressMM.AddMessage('  Writing SGM');
  DataArray := Model.DataArrayManager.GetDataSetByName(KMoistSpecificGravi);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'SGM');
  WritePestZones(DataArray, FInputFileName, CCUB_SGM, 'SGM');

  frmProgressMM.AddMessage('  Writing SGS');
  DataArray := Model.DataArrayManager.GetDataSetByName(KSaturatedSpecificG);
  Assert(DataArray <> nil);
  WriteMf6_DataSet(DataArray, 'SGS');
  WritePestZones(DataArray, FInputFileName, CSUB_SGS, 'SGS');

  WriteEndGridData;
end;

procedure TCSubWriter.WriteOptions;
var
  OutputFileName: string;
  NameOfFile: string;
  CsvFile: string;
begin
  WriteBeginOptions;

//[BOUNDNAMES]
  WriteString('  BOUNDNAMES');
  NewLine;

//[PRINT_INPUT]
  PrintListInputOption;

  //[SAVE_FLOWS]
  WriteSaveFlowsOption;

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
  if not FCSubPackage.HeadBased and FCSubPackage.PreconsolidationHeadUsed then
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
    OutputFileName := ChangeFileExt(FFileName, StrCsubstrncsv);
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [STRAIN_CSV_COARSE FILEOUT <coarsestrain_filename>]
  if coCourseStrain in FCSubPackage.OutputTypes then
  begin
    WriteString('  STRAIN_CSV_COARSE FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, StrCsubcrsstrncsv);
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION FILEOUT <compaction_filename>]
  if coCompaction in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, StrCsubcmpct);
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION_ELASTIC FILEOUT <elastic_compaction_filename>]
  if coElasticComp in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION_ELASTIC FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, StrCsubelstcmpct);
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION_INELASTIC FILEOUT <inelastic_compaction_filename>]
  if coInelasticComp in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION_INELASTIC FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, StrCsubinelstcmpct);
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION_INTERBED FILEOUT <interbed_compaction_filename>]
  if coInterbedComp in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION_INTERBED FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, StrCsubintrbdcmpct);
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [COMPACTION_COARSE FILEOUT <coarse_compaction_filename>]
  if coCoarseComp in FCSubPackage.OutputTypes then
  begin
    WriteString('  COMPACTION_COARSE FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, StrCsubcrscmpct);
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

//  [ZDISPLACEMENT FILEOUT <zdisplacement_filename>]
  if coZDisplacement in FCSubPackage.OutputTypes then
  begin
    WriteString('  ZDISPLACEMENT FILEOUT ');
    OutputFileName := ChangeFileExt(FFileName, StrCsubzdis);
    Model.ModelOutputFiles.Add(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    NewLine;
  end;

  if FCSubPackage.WriteConvergenceData then
  begin
    WriteString('  PACKAGE_CONVERGENCE FILEOUT ');
    CsvFile := ChangeFileExt(FFileName, '.CsubConvergence.CSV');
    Model.AddModelOutputFile(CsvFile);
    CsvFile := ExtractFileName(CsvFile);
    WriteString(CsvFile);
    NewLine;
  end;

  WriteTimeSeriesFiles(FInputFileName);

  if FObservations.Count > 0 then
  begin
    WriteString('    OBS6 FILEIN ');
    NameOfFile := ChangeFileExt(FFileName, ObservationExtension);
    Model.AddModelInputFile(NameOfFile);
    NameOfFile := ExtractFileName(NameOfFile);
    WriteString(NameOfFile);
    NewLine;
  end;

//  [TS6 FILEIN <ts6_filename>]
//  [OBS6 FILEIN <obs6_filename>]

  WriteEndOptions
end;

procedure TCSubWriter.WritePackageData;
var
  InterbedIndex: Integer;
  Interbed: TCSubInterbed;
  DataArrayManager: TDataArrayManager;
  cdelay: string;
  pcsDataArray: TDataArray;
  thick_fracDataArray: TDataArray;
  rnbDataArray: TDataArray;
  ssv_ccDataArray: TDataArray;
  sse_crDataArray: TDataArray;
  thetaDataArray: TDataArray;
  kvDataArray: TDataArray;
  h0DataArray: TDataArray;
  CSubDataArray: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DisvUsed: Boolean;
  icsubno: Integer;
  IDomain: TDataArray;
  InterbedSystemCount: Integer;
  IbIndex: Integer;
  boundname: string;
begin
  if ninterbeds = 0 then
  begin
    Exit;
  end;
  SetLength(FInterBedNumbers, Model.LayerCount, Model.RowCount, Model.ColumnCount);

  WriteBeginPackageData;
  DisvUsed := Model.DisvUsed;
  icsubno := 0;

  DataArrayManager := Model.DataArrayManager;
  IDomain := DataArrayManager.GetDataSetByName(K_IDOMAIN);
  InterbedSystemCount := FCSubPackage.Interbeds.Count;

  for InterbedIndex := 0 to FCSubPackage.Interbeds.Count - 1 do
  begin
    Interbed := FCSubPackage.Interbeds[InterbedIndex];
    case Interbed.InterbedType of
      itDelay: cdelay := '   DELAY';
      itNoDelay: cdelay := ' NODELAY';
    end;
// <icsubno> <cellid(ncelldim)> <pcs> <pcs0> <thick_fracDataArray> <rnbDataArray> <ssv_ccDataArray> <sse_crDataArray> <thetaDataArray> <kvDataArray> <h0DataArray> [<boundname>]
    pcsDataArray := DataArrayManager.GetDataSetByName(Interbed.InitialOffset);
    pcsDataArray.Initialize;

    thick_fracDataArray := DataArrayManager.GetDataSetByName(Interbed.Thickness);
    thick_fracDataArray.Initialize;

    if Interbed.InterbedType = itDelay then
    begin
      rnbDataArray := DataArrayManager.GetDataSetByName(Interbed.EquivInterbedNumberName);
      rnbDataArray.Initialize;
    end
    else
    begin
      rnbDataArray := nil;
    end;

    ssv_ccDataArray := DataArrayManager.GetDataSetByName(Interbed.InitialInelasticSpecificStorage);
    ssv_ccDataArray.Initialize;

    sse_crDataArray := DataArrayManager.GetDataSetByName(Interbed.InitialElasticSpecificStorage);
    sse_crDataArray.Initialize;
    thetaDataArray := DataArrayManager.GetDataSetByName(Interbed.InitialPorosity);
    thetaDataArray.Initialize;
    if Interbed.InterbedType = itDelay  then
    begin
      kvDataArray := DataArrayManager.GetDataSetByName(Interbed.DelayKvName);
      kvDataArray.Initialize;
      h0DataArray := DataArrayManager.GetDataSetByName(Interbed.InitialDelayHeadOffset);
      h0DataArray.Initialize;
    end
    else
    begin
      kvDataArray := nil;
      h0DataArray := nil;
    end;

    CSubDataArray := DataArrayManager.GetDataSetByName(Interbed.CSubBoundName);
    CSubDataArray.Initialize;

    for LayerIndex := 0 to Model.LayerCount - 1 do
    begin
      for RowIndex := 0 to Model.RowCount - 1 do
      begin
        for ColIndex := 0 to Model.ColumnCount - 1 do
        begin
          if pcsDataArray.IsValue[LayerIndex, RowIndex, ColIndex]
            and (IDomain.IntegerData[LayerIndex, RowIndex, ColIndex] > 0) then
          begin
            Inc(icsubno);
            if FInterBedNumbers[LayerIndex, RowIndex, ColIndex] = nil then
            begin
              SetLength(FInterBedNumbers[LayerIndex, RowIndex, ColIndex], InterbedSystemCount);
              for IbIndex := 0 to InterbedSystemCount - 1 do
              begin
                FInterBedNumbers[LayerIndex, RowIndex, ColIndex, IbIndex] := 0;
              end;
            end;
            FInterBedNumbers[LayerIndex, RowIndex, ColIndex, InterbedIndex] := icsubno;

            boundname := ' ' + CSubDataArray.StringData[LayerIndex, RowIndex, ColIndex];

            WriteString('  ');
            WriteInteger(icsubno);
            WriteInteger(LayerIndex+1);
            if not DisvUsed then
            begin
              WriteInteger(RowIndex+1);
            end;
            WriteInteger(ColIndex+1);
            WriteString(cdelay);

//            WriteFloat(pcs);
            WriteDataArrayValueOrFormula(pcsDataArray, LayerIndex, RowIndex, ColIndex);

//            WriteFloat(thick_frac);
            WriteDataArrayValueOrFormula(thick_fracDataArray, LayerIndex, RowIndex, ColIndex);

            if rnbDataArray <> nil then
            begin
//              WriteFloat(rnb);
              WriteDataArrayValueOrFormula(rnbDataArray, LayerIndex, RowIndex, ColIndex);
            end
            else
            begin
              WriteFloat(1);
            end;

//            WriteFloat(ssv_cc);
            WriteDataArrayValueOrFormula(ssv_ccDataArray, LayerIndex, RowIndex, ColIndex);

//            WriteFloat(sse_cr);
            WriteDataArrayValueOrFormula(sse_crDataArray, LayerIndex, RowIndex, ColIndex);

//            WriteFloat(theta);
            WriteDataArrayValueOrFormula(thetaDataArray, LayerIndex, RowIndex, ColIndex);

            if kvDataArray <> nil then
            begin
//              WriteFloat(kv);
              WriteDataArrayValueOrFormula(kvDataArray, LayerIndex, RowIndex, ColIndex);
            end
            else
            begin
              WriteFloat(999);
            end;

            if h0DataArray <> nil then
            begin
//              WriteFloat(h0);
              WriteDataArrayValueOrFormula(kvDataArray, LayerIndex, RowIndex, ColIndex);
            end
            else
            begin
              WriteFloat(999);
            end;

            WriteString(boundname);
            NewLine;
          end;
        end;
      end;
    end;
  end;

  WriteEndPackageData
end;

procedure TCSubWriter.WriteStressPeriods;
var
  ITMP: Integer;
  List: TValueCellList;
  TimeIndex: Integer;
begin
  for TimeIndex := 0 to Values.Count - 1 do
  begin
    FStressPeriod := TimeIndex;
    FBoundaryIndex := 0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(Format(StrWritingStressPer, [TimeIndex+1]));
    GetITMP(ITMP, TimeIndex, List);
    if (ITMP = 0) then
    begin
      frmErrorsAndWarnings.AddWarning(Model,
        Format(StrNoBoundaryConditio1, [Package.PackageIdentifier]),
        Format(StrStressPeriod0d, [TimeIndex+1]));
    end;

    if (ITMP < 0) then
    begin
      Continue;
    end;
    WriteBeginPeriod(TimeIndex);

    if ITMP > 0 then
    begin
      WriteAndCheckCells(List, TimeIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;
    WriteEndPeriod;

    if TimeIndex = Values.Count - 1 then
    begin
      if List <> nil then
      begin
        List.Cache;
      end;
    end;
  end;
end;

end.
