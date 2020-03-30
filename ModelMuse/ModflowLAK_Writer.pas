unit ModflowLAK_Writer;

interface

uses SysUtils, Classes, PhastModelUnit, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, ModflowTimeUnit, ModflowLakUnit;

type
  TModflowLAK_Writer = class(TCustomPackageWriter)
  private
    // @name contains all the @link(TScreenObject)s that define lakes.
    FLakeList: TList;
    FNameOfFile: string;
    FPackage: TLakePackageSelection;
    FLakeObservationsUsed: Boolean;
    procedure WriteDataSet1a;
    procedure WriteDataSet1b;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSets4To9;
    procedure WriteLakeDefinitions;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSets7And8;
    procedure WriteDataSet9(StressPeriod: TModflowStressPeriod);
    procedure WriteGages(Lines: TStrings);
    procedure WriteObsScript(const AFileName: string);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure Evaluate;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string; Lines: TStrings);
  end;

  TExternalBathymetryFileWriter = class(TCustomModflowWriter)
  private
    FExternalBathymetry: TExternalLakeTable;
    FScreenObject: TObject;
    FFileName: string;
    FLakeID: Integer;
  protected
    class function Extension: string; override;
    constructor Create(AModel: TCustomModel;
      ExternalBathymetry: TExternalLakeTable;
      ScreenObject: TObject; LakeID: integer); reintroduce;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses ModflowUnitNumbers, ScreenObjectUnit, frmErrorsAndWarningsUnit,
  DataSetUnit, frmProgressUnit, Forms, GoPhastTypes, PestObsUnit;

resourcestring
  DupNameErrorMessage = 'The following Lakes have the same Lake ID.';
  InvalidCenterLake = 'The follow lakes have invalid center lake numbers.';
  StrSAndS = '%0:s and %1:s';
  StrTheLakeBathymetry = 'The Lake bathymetry table must have exactly 151 en' +
  'tries. The lakes defined by the following objects have too many or too fe' +
  'w entries.';
  StrLakeBathymetryFile = 'Lake Bathymetry file does not exist.';
  StrTheLakeBathymetry2 = 'The lake bathymetry file (%0:s) specified in objec' +
  't %1:s does not exist';
  StrEvaluatingLAKPacka = 'Evaluating LAK Package data.';
  StrWritingLAKPackage = 'Writing LAK Package input.';
  StrWritingDataSet1a = '  Writing Data Set 1a.';
  StrWritingDataSet1b = '  Writing Data Set 1b.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
  StrWritingDataSets4to9 = '  Writing Data Sets 4 to 9.';
  StrNoCellsAssociated = 'No cells associated with lake. If the objects only ' +
  'set values of enclosed cells, try having them set values of intersected ' +
  'cells.';
  StrThereAreNoCellsAObj = 'There are no cells associated with the lake defi' +
  'ned by %s.';

{ TModflowLAK_Writer }

constructor TModflowLAK_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FLakeList := TList.Create;
end;

destructor TModflowLAK_Writer.Destroy;
begin
  FLakeList.Free;
  inherited;
end;

function SortLakes(Item1, Item2: Pointer): Integer;
var
  Lake1, Lake2: TScreenObject;
begin
  Lake1 := Item1;
  Lake2 := Item2;
  Assert(Lake1.ModflowLakBoundary <> nil);
  Assert(Lake2.ModflowLakBoundary <> nil);
  result := Lake1.ModflowLakBoundary.LakeID - Lake2.ModflowLakBoundary.LakeID;
end;

procedure TModflowLAK_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject, OtherObject: TScreenObject;
  TempList: TList;
  SubLakeIndex: Integer;
  CellList: TCellAssignmentList;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, DupNameErrorMessage);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, InvalidCenterLake);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheLakeBathymetry);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeBathymetryFile);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoCellsAssociated);

    frmProgressMM.AddMessage(StrEvaluatingLAKPacka);
    TempList := TList.Create;
    try
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
        if (ScreenObject.ModflowLakBoundary <> nil)
          and ScreenObject.ModflowLakBoundary.Used
          and (ScreenObject.ModflowLakBoundary.LakeID > 0) then
        begin
          CellList := TCellAssignmentList.Create;
          try
            ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
            if CellList.Count = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, StrNoCellsAssociated,
                Format(StrThereAreNoCellsAObj, [ScreenObject.Name]),
                ScreenObject);
            end;
          finally
            CellList.Free
          end;

          While (FLakeList.Count <= ScreenObject.ModflowLakBoundary.LakeID) do
          begin
            FLakeList.Add(nil);
          end;
          if FLakeList[ScreenObject.ModflowLakBoundary.LakeID] <> nil then
          begin
            OtherObject := FLakeList[ScreenObject.ModflowLakBoundary.LakeID];
            frmErrorsAndWarnings.AddError(Model, DupNameErrorMessage,
              Format(StrSAndS, [OtherObject.Name, ScreenObject.Name]),
              ScreenObject);
          end;
          FLakeList[ScreenObject.ModflowLakBoundary.LakeID]
            := ScreenObject;
          TempList.Add(ScreenObject);
          ScreenObject.ModflowLakBoundary.ClearSubLakes;
        end;
      end;
      for ScreenObjectIndex := 0 to TempList.Count - 1 do
      begin
        ScreenObject := TempList[ScreenObjectIndex];
        Assert(ScreenObject.ModflowLakBoundary <> nil);
        ScreenObject.ModflowLakBoundary.TrueLakeID := -1;
        if ScreenObject.ModflowLakBoundary.CenterLake < FLakeList.Count then
        begin
          if ScreenObject.ModflowLakBoundary.CenterLake <> 0 then
          begin
            OtherObject := FLakeList[ScreenObject.ModflowLakBoundary.CenterLake];
            if OtherObject = nil then
            begin
              frmErrorsAndWarnings.AddError(Model, InvalidCenterLake,
                ScreenObject.Name, ScreenObject)
            end
            else
            begin
              OtherObject.ModflowLakBoundary.AddSubLake(
                ScreenObject);
            end;
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, InvalidCenterLake,
            ScreenObject.Name, ScreenObject)
        end;
      end;
      FLakeList.Pack;
      FLakeList.Sort(SortLakes);
      for ScreenObjectIndex := 0 to FLakeList.Count - 1 do
      begin
        ScreenObject := FLakeList[ScreenObjectIndex];
        Assert(ScreenObject.ModflowLakBoundary <> nil);
        ScreenObject.ModflowLakBoundary.TrueLakeID := ScreenObjectIndex + 1;
      end;
      for ScreenObjectIndex := 0 to FLakeList.Count - 1 do
      begin
        ScreenObject := FLakeList[ScreenObjectIndex];
        Assert(ScreenObject.ModflowLakBoundary <> nil);
        for SubLakeIndex :=
          ScreenObject.ModflowLakBoundary.SubLakeCount - 1 downto 0 do
        begin
          OtherObject := ScreenObject.ModflowLakBoundary.
            SubLakes[SubLakeIndex] as TScreenObject;
          if OtherObject.ModflowLakBoundary.TrueLakeID < 0 then
          begin
            ScreenObject.ModflowLakBoundary.DeleteSubLake(SubLakeIndex);
          end;
        end;
      end;
    finally
      TempList.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowLAK_Writer.Extension: string;
begin
  result := '.lak';
end;

function TModflowLAK_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.LakPackage;
end;

procedure TModflowLAK_Writer.WriteDataSet1a;
begin
  if FPackage.ExternalLakeChoice = elcAll then
  begin
    WriteString('TABLEINPUT # DataSet 1a');
    NewLine;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSet1b;
var
  NLAKES: integer;
  ILKCB: integer;
begin
  NLAKES := FLakeList.Count;
  GetFlowUnitNumber(ILKCB);

  WriteInteger(NLAKES);
  WriteInteger(ILKCB);
  WriteString(' # DataSet 1b: NLAKES ILKCB');
  NewLine;
end;

procedure TModflowLAK_Writer.WriteDataSet2;
var
  THETA: double;
  NSSITR: integer;
  SSCNCR: double;
  LakePkg: TLakePackageSelection;
  SURFDEPTH: double;
begin
  LakePkg := Model.ModflowPackages.LakPackage;
  THETA := LakePkg.Theta;
//  if PhastModel.ModflowFullStressPeriods.CompletelyTransient then
//  begin
    // Make Theta negative to force NSSITR and SSCNCR to be read.
    // Make Theta negative even for steady state models to force SURFDEPTH to be read
    THETA := -THETA;
//  end;
  NSSITR := LakePkg.NumberOfIterations;
  SSCNCR := LakePkg.ConvergenceCriterion;
  SURFDEPTH := LakePkg.SurfDepth.Value;

  WriteFloat(THETA);
  WriteInteger(NSSITR);
  WriteFloat(SSCNCR);
  WriteFloat(SURFDEPTH);
  WriteString(' # DataSet 2: THETA NSSITR SSCNCR SURFDEPTH');
  NewLine;
end;

procedure TModflowLAK_Writer.WriteDataSet3;
var
  STAGES, SSMN, SSMX: double;
  FirstPeriodIsSteadyState: boolean;
  LakeIndex: Integer;
  ScreenObject: TScreenObject;
  Lake: TLakBoundary;
  LakeTime: TLakItem;
  IUNITLAKTAB: integer;
  LakeTableFileName: string;
  BathymWriter: TExternalBathymetryFileWriter;
  BathymFileName: string;
begin
  FirstPeriodIsSteadyState :=
    Model.ModflowFullStressPeriods[0].StressPeriodType = sptSteadyState;
  for LakeIndex := 0 to FLakeList.Count - 1 do
  begin
    ScreenObject := FLakeList[LakeIndex];
    Assert(ScreenObject.ModflowLakBoundary <> nil);
    Lake := ScreenObject.ModflowLakBoundary;
    if FPackage.ExternalLakeChoice = elcAll then
    begin
      IUNITLAKTAB := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
      case Lake.ExternalLakeTable.LakeTableChoice of
        lctInternal:
          begin
            BathymFileName := ChangeFileExt(FNameOfFile, '');
            BathymFileName := TExternalBathymetryFileWriter.FileName(BathymFileName) + IntToStr(Lake.TrueLakeID);
            WriteToNameFile(StrData, IUNITLAKTAB, BathymFileName, foInput, Model, False);
            BathymWriter := TExternalBathymetryFileWriter.Create(Model,
              Lake.ExternalLakeTable, ScreenObject, Lake.TrueLakeID);
            try
              BathymWriter.WriteFile(BathymFileName);
            finally
              BathymWriter.Free;
            end;
          end;
        lctExternal:
          begin
            LakeTableFileName := ExtractRelativePath(FNameOfFile,
              Lake.ExternalLakeTable.FullLakeTableFileName);
            if not FileExists(LakeTableFileName) then
            begin
              frmErrorsAndWarnings.AddError(Model,StrLakeBathymetryFile,
                Format(StrTheLakeBathymetry2,
                [LakeTableFileName, ScreenObject.Name]), ScreenObject);
            end;
            WriteToNameFile(StrData, IUNITLAKTAB, LakeTableFileName,
              foInputAlreadyExists, Model, True);
          end
        else Assert(False);
      end;
//      Inc(StartUnitNumber);
    end
    else
    begin
      IUNITLAKTAB := -1;
    end;
    STAGES := Lake.InitialStage;
    WriteFloat(STAGES);
    if FirstPeriodIsSteadyState then
    begin
      LakeTime := Lake.Values[0] as TLakItem;
      SSMN := LakeTime.SSMN;
      SSMX := LakeTime.SSMX;

      WriteFloat(SSMN);
      WriteFloat(SSMX);
      if IUNITLAKTAB > 0 then
      begin
        WriteInteger(IUNITLAKTAB);
      end;
      WriteString(' # DataSet 3: STAGES SSMN SSMX');
      if IUNITLAKTAB > 0 then
      begin
        WriteString(' IUNITLAKTAB');
      end;
    end
    else
    begin
      if IUNITLAKTAB > 0 then
      begin
        WriteInteger(IUNITLAKTAB);
      end;
      WriteString(' # DataSet 3: STAGES');
      if IUNITLAKTAB > 0 then
      begin
        WriteString(' IUNITLAKTAB');
      end;
    end;
    NewLine;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSet5;
var
  LakeID: TDataArray;
  LayerIndex: integer;
  ModflowLayer: integer;
begin
  LakeID := Model.DataArrayManager.GetDataSetByName(rsLakeID);
  ModflowLayer := 0;
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Inc(ModflowLayer);
      WriteArray(LakeID, LayerIndex, 'Data Set 5, LKARR: Layer '
        + IntToStr(ModflowLayer), StrNoValueAssigned, 'LKARR');
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSet6;
var
  LakeLeakance: TDataArray;
  LayerIndex: integer;
  ModflowLayer: integer;
begin
  LakeLeakance := Model.DataArrayManager.GetDataSetByName(rsLakeLeakance);
  ModflowLayer := 0;
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Inc(ModflowLayer);
      WriteArray(LakeLeakance, LayerIndex,
        'Data Set 6, BDLKNC: Layer ' + IntToStr(ModflowLayer),
        StrNoValueAssigned, 'BDLKNC');
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSet9(StressPeriod: TModflowStressPeriod);
var
  LakeIndex: Integer;
  ScreenObject: TScreenObject;
  Lake: TLakBoundary;
  TimeIndex: Integer;
  LakeItem: TLakItem;
  PRCPLK, EVAPLK, RNF, WTHDRW, SSMN, SSMX: double;
begin
  for LakeIndex := 0 to FLakeList.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := FLakeList[LakeIndex];
    Assert(ScreenObject.ModflowLakBoundary <> nil);
    Lake := ScreenObject.ModflowLakBoundary;
    for TimeIndex := 0 to Lake.Values.Count -1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      LakeItem := Lake.Values[TimeIndex] as TLakItem;
      if (LakeItem.StartTime <= StressPeriod.StartTime)
        and (LakeItem.EndTime > StressPeriod.StartTime) then
      begin
        PRCPLK := LakeItem.PRCPLK;
        EVAPLK := LakeItem.EVAPLK;
        RNF := LakeItem.RNF;
        WTHDRW := LakeItem.WTHDRW;

        WriteFloat(PRCPLK);
        WriteFloat(EVAPLK);
        WriteFloat(RNF);
        WriteFloat(WTHDRW);

        if StressPeriod.StressPeriodType = sptSteadyState then
        begin
          SSMN := LakeItem.SSMN;
          SSMX := LakeItem.SSMX;
          WriteFloat(SSMN);
          WriteFloat(SSMX);
        end;
        WriteString(' # DataSet 9: PRCPLK EVAPLK RNF WTHDRW');
        if StressPeriod.StressPeriodType = sptSteadyState then
        begin
          WriteString(' SSMN SSMX');
        end;
        NewLine;
        break;
      end;
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSets4To9;
var
  TimeIndex: integer;
  StressPeriod: TModflowStressPeriod;
  ITMP, ITMP1, LWRT: integer;
begin
  ITMP1 := 1;
  if Model.ModflowPackages.LakPackage.PrintLakes then
  begin
    LWRT := 0;
  end
  else
  begin
    LWRT := 1;
  end;
  for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    // data set 4;
    if TimeIndex = 0 then
    begin
      ITMP := 1;
    end
    else
    begin
      ITMP := -1;
    end;
    WriteInteger(ITMP);
    WriteInteger(ITMP1);
    WriteInteger(LWRT);
    WriteString(' # DataSet 4: ITMP ITMP1 LWRT');
    NewLine;

    if TimeIndex = 0 then
    begin
      WriteLakeDefinitions;
    end;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
    WriteDataSet9(StressPeriod);
  end;
end;

procedure TModflowLAK_Writer.WriteDataSets7And8;
var
  LakeIndex: integer;
  ScreenObject, SubScreenObject: TScreenObject;
  Lake, SubLake: TLakBoundary;
  SubLakeIndex: Integer;
  IC: integer;
  NSLMS: integer;
begin
  // data set 7
  NSLMS := 0;
  for LakeIndex := 0 to FLakeList.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := FLakeList[LakeIndex];
    Lake := ScreenObject.ModflowLakBoundary;
    Assert(ScreenObject.ModflowLakBoundary <> nil);
    if Lake.SubLakeCount > 0 then
    begin
      Inc(NSLMS);
    end;
  end;
  WriteInteger(NSLMS);
  WriteString(' # DataSet 7: NSLMS');
  NewLine;

  if NSLMS > 0 then
  begin
    for LakeIndex := 0 to FLakeList.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ScreenObject := FLakeList[LakeIndex];
      Assert(ScreenObject.ModflowLakBoundary <> nil);
      Lake := ScreenObject.ModflowLakBoundary;
      if Lake.SubLakeCount > 0 then
      begin
        // Data Set 8a
        IC := Lake.SubLakeCount+1;
        WriteInteger(IC);
        WriteInteger(Lake.TrueLakeID);
        for SubLakeIndex := 0 to Lake.SubLakeCount - 1 do
        begin
          SubScreenObject := Lake.SubLakes[SubLakeIndex] as TScreenObject;
          Assert(SubScreenObject.ModflowLakBoundary <> nil);
          SubLake := SubScreenObject.ModflowLakBoundary;
          WriteInteger(SubLake.TrueLakeID);
        end;
        WriteString(' # DataSet 8a: IC ISUB(1) ISUB(2) ............ ISUB(IC)');
        NewLine;

        // Data Set 8b
        for SubLakeIndex := 0 to Lake.SubLakeCount - 1 do
        begin
          SubScreenObject := Lake.SubLakes[SubLakeIndex] as TScreenObject;
          Assert(SubScreenObject.ModflowLakBoundary <> nil);
          SubLake := SubScreenObject.ModflowLakBoundary;
          WriteFloat(SubLake.Sill);
        end;
        WriteString(' # DataSet 8b: SILLVT(2) ............. SILLVT(IC)');
        NewLine;
      end;
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteFile(const AFileName: string;
  Lines: TStrings);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrLAK) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  FPackage := Package as TLakePackageSelection;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrLAK, Model.UnitNumbers.UnitNumber(StrLAK), FNameOfFile, foInput, Model);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  Evaluate;
  OpenFile(FileName(AFileName));
  try
    frmProgressMM.AddMessage(StrWritingLAKPackage);
    frmProgressMM.AddMessage(StrWritingDataSet1a);
    WriteDataSet1a;
      Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet1b);
    WriteDataSet1b;
      Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2);
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet3);
    Application.ProcessMessages;
    WriteDataSet3;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets4to9);
    WriteDataSets4To9;
  finally
    CloseFile;
  end;
  WriteGages(Lines);
  WriteObsScript(AFileName);
end;

procedure TModflowLAK_Writer.WriteGages(
  Lines: TStrings);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  LAKE: integer;
  UNIT_Number: integer;
  OUTTYPE: integer;
  Line: string;
  OutputName: string;
  procedure WriteGage;
  begin
    LAKE := -ScreenObject.ModflowLakBoundary.TrueLakeID;
    UNIT_Number := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
    Line := IntToStr(LAKE) + ' '
      + IntToStr(-UNIT_Number) + ' '
      + IntToStr(OUTTYPE);
    Lines.Add(Line);
//    Inc(StartUnitNumber);
    OutputName := ChangeFileExt(FNameOfFile, '.lakg');
    OutputName := OutputName + IntToStr(Lines.Count);
    WriteToNameFile(StrDATA, UNIT_Number, OutputName, foOutput, Model);
    ScreenObject.ModflowLakBoundary.Observations.GageOutputName := OutputName;
  end;
begin
  FLakeObservationsUsed := False;
  for ScreenObjectIndex := 0 to FLakeList.Count - 1 do
  begin
    ScreenObject := FLakeList[ScreenObjectIndex];
    Assert((ScreenObject.ModflowLakBoundary <> nil)
      and ScreenObject.ModflowLakBoundary.Used);
    OUTTYPE := ScreenObject.ModflowLakBoundary.OutType;
    if OUTTYPE >= 0 then
    begin
      WriteGage;
    end;
    if ScreenObject.ModflowLakBoundary.Gage4
      or (ScreenObject.ModflowLakBoundary.Observations.Count > 0) then
    begin
      FLakeObservationsUsed := True;
      OUTTYPE := 4;
      WriteGage;
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteLakeDefinitions;
begin
  WriteDataSet5;
  WriteDataSet6;
  WriteDataSets7And8;
end;

procedure TModflowLAK_Writer.WriteObsScript(const AFileName: string);
var
  StartTime: Double;
  ScriptFileName: string;
  ComparisonsUsed: Boolean;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Observations: TLakeObservations;
  ObsIndex: Integer;
  Obs: TLakeObs;
  CompIndex: Integer;
  CompItem: TObsCompareItem;
  function GetObName(ObjectIndex: Integer; Obs: TCustomObservationItem): string;
  begin
    Result := PrefixedObsName('Lak', ObjectIndex, Obs);
  end;
begin
{$IFNDEF PEST}
  Exit;
{$ENDIF}
  if not FLakeObservationsUsed then
  begin
    Exit;
  end;

  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  ScriptFileName := ChangeFileExt(AFileName, '.Lake_script');

  OpenFile(ScriptFileName);
  try
    ComparisonsUsed := False;
    // OBSERVATIONS block
    WriteString('BEGIN OBSERVATIONS');
    NewLine;
    for ScreenObjectIndex := 0 to FLakeList.Count - 1 do
    begin
      ScreenObject := FLakeList[ScreenObjectIndex];
      Observations := ScreenObject.ModflowLakBoundary.Observations;
      if Observations.Count > 0 then
      begin
        WriteString('  # ');
        WriteString('Observations defined in ');
        WriteString(ScreenObject.Name);
        NewLine;
        WriteString('  FILENAME ');
        WriteString(Observations.GageOutputName);
        NewLine;

        for ObsIndex := 0 to Observations.Count - 1 do
        begin
          Obs := Observations[ObsIndex];
  //          FObsItemDictionary.Add(Obs.GUID, Obs);
          WriteString('  OBSERVATION ');
          WriteString(GetObName(ScreenObjectIndex, Obs));
          WriteString(' ');
          WriteString(Obs.ObservationType);
          WriteFloat(Obs.Time - StartTime);
          WriteFloat(Obs.ObservedValue);
          WriteFloat(Obs.Weight);
          WriteString(' PRINT');
          NewLine;
        end;

        if Observations.Comparisons.Count > 0 then
        begin
          ComparisonsUsed := True;
        end;
      end;
    end;
    WriteString('END OBSERVATIONS');

    // DERIVED_OBSERVATIONS block
    if ComparisonsUsed then
    begin
      NewLine;
      NewLine;
      WriteString('BEGIN DERIVED_OBSERVATIONS');
      NewLine;

      for ScreenObjectIndex := 0 to FLakeList.Count - 1 do
      begin
        ScreenObject := FLakeList[ScreenObjectIndex];
        Observations := ScreenObject.ModflowLakBoundary.Observations;
        if Observations.Comparisons.Count > 0 then
        begin
          WriteString('  # ');
          WriteString('Observation comparisons defined in ');
          WriteString(ScreenObject.Name);
          NewLine;
        end;

        for CompIndex := 0 to Observations.Comparisons.Count - 1 do
        begin
          WriteString('  DIFFERENCE ');
          CompItem := Observations.Comparisons[CompIndex];
          WriteString(GetObName(ScreenObjectIndex, CompItem));
          WriteString(' ');
          Obs := Observations[CompItem.Index1];
          WriteString(Obs.ExportedName);
          WriteString(' ');
          Obs := Observations[CompItem.Index2];
          WriteString(Obs.ExportedName);
          WriteFloat(CompItem.ObservedValue);
          WriteFloat(CompItem.Weight);
          WriteString(' PRINT');
          NewLine;
        end;
      end;
      WriteString('END DERIVED_OBSERVATIONS');
    end;
  finally
    CloseFile;
  end;
end;

{ TExternalBathymetryFileWriter }

constructor TExternalBathymetryFileWriter.Create(AModel: TCustomModel;
  ExternalBathymetry: TExternalLakeTable; ScreenObject: TObject; LakeID: integer);
begin
  inherited Create(AModel, etExport);
  FExternalBathymetry := ExternalBathymetry;
  Assert(ScreenObject is TScreenObject);
  FScreenObject := ScreenObject;
  FLakeID := LakeID;
end;

class function TExternalBathymetryFileWriter.Extension: string;
begin
  result := '.lak_bath';
end;

procedure TExternalBathymetryFileWriter.WriteFile(const AFileName: string);
var
  LakeTable: TLakeTable;
  index: Integer;
  Item: TLakeTableItem;
begin
  Assert(FExternalBathymetry.LakeTableChoice = lctInternal);
  LakeTable := FExternalBathymetry.LakeTable;
  if LakeTable.Count <> 151 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrTheLakeBathymetry,
      (FScreenObject as TScreenObject).Name, FScreenObject);
  end;
  FFileName := FileName(AFileName) + IntToStr(FLakeID);
  OpenFile(FFileName);
  try
    for index := 0 to LakeTable.Count - 1 do
    begin
      Item := LakeTable[index];
      WriteFloat(Item.Stage);
      WriteFloat(Item.Volume);
      WriteFloat(Item.SurfaceArea);
      NewLine;
    end;
  finally
    CloseFile;
  end;
end;

end.
