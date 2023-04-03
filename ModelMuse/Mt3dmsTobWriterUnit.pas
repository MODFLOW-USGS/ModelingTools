unit Mt3dmsTobWriterUnit;

interface

uses System.UITypes,
  Winapi.Windows, CustomModflowWriterUnit, GoPhastTypes, PhastModelUnit, Classes, SysUtils,
  Mt3dmsTobUnit, Generics.Collections, Mt3dmsFluxObservationsUnit, RbwParser,
  ScreenObjectUnit, FluxObservationUnit, ModflowPackageSelectionUnit, Forms,
  ModflowBoundaryDisplayUnit, Vcl.Dialogs;

type
  TObsList = TList<TMt3dmsFluxObservationGroup>;

  TMt3dmsTobWriter = class(TCustomPackageWriter)
  private
    FNameOfFile: string;
    // @name contains @link(TMt3dmsTransObservations).
    FObservations: TList;
    FStartTime: Double;
    FEndTime: Double;
    nConcObs: integer;
    FTobPackage: TMt3dmsTransportObservations;
    OUTNAM: string;
    FFluxObsList: TObsList;
    FMaxConcObs: Integer;
    FMaxFluxObs: Integer;
    inConcObs: Integer;
    inFluxObs: Integer;
    procedure EvaluateConcentrationObs(Purpose: TObservationPurpose);
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3to5;
    procedure WriteDatSets4and5(Index: integer);
    procedure WriteDataSet4(Observations: TMt3dmsTransObservations;
      CellList: TMt3dmsTobsCellList);
    procedure WriteDataSet5(Observations: TMt3dmsTransObservations;
      CellList: TMt3dmsTobsCellList);
    procedure WriteFluxObservations;
    procedure WriteDataSets7to9(ObsGroup: TMt3dmsFluxObservationGroup);
    procedure WriteFluxObsTime(const FOBSNAM: string;
      TimeItem: TMt3dmsFluxObservation);
    procedure WriteFluxObsCell(DataSets: TList;
      ACell: TCellAssignment; Expression: TExpression;
      Variables: TList;
      ObsFactor: TObservationFactor);
    procedure WriteDataSet7(ObsGroup: TMt3dmsFluxObservationGroup);
    procedure WriteDataSet8(ObsGroup: TMt3dmsFluxObservationGroup);
    procedure WriteDataSet9(ObsGroup: TMt3dmsFluxObservationGroup);
    procedure FillFluxObsList;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string; Purpose: TObservationPurpose);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists;
      ParameterIndicies: TByteSet; Purpose: TObservationPurpose);
  end;

implementation

uses
  frmErrorsAndWarningsUnit, DataSetUnit, Contnrs,
  SubscriptionUnit, GlobalVariablesUnit, GIS_Functions, frmFormulaErrorsUnit,
  ModflowUnitNumbers, frmProgressUnit, Math, ModflowBoundaryUnit;

resourcestring
  ConcOffGrid = 'One or more concentration observation are not located on ' +
    'the grid and will be ignored';
  StrNoValidConcObserv = 'No valid concentration observations were defined. ' +
  'Check that "Model|Observation Type" is set to the correct value and that ' +
  'the observation type for each observation is set correctly.';
  StrNoConcentrationObs = 'No concentration observations';
  StrNoValidConcPred = 'No valid concentration observations were defined for ' +
  'the current stress period. Check that "Model|Observation Type" is set to ' +
  'the correct value and that the observation type for each observation is ' +
  'set correctly.';
  StrNoValidConcObs = 'No valid concentration observations were defined ' +
    'for the current stress period.';
  StrNoValidConcPredForCurrent = 'No valid concentration predictions were ' +
  'defined for the current stress period.';
  InvalidConcEndObsTime = 'Concentration observation time after end of simulation';
  InvalidConcStartObsTime = 'Concentration observation time before ' +
    'beginning of simulation';
  MissingConcObsNameError = 'The concentration observation in the following ' +
    'objects do not have observations names assigned';
  ConcObsNameWarning = 'The following Transport observation names may be '
    + 'valid for MT3DMS or MT3D-USGS but they are not valid for UCODE.';

  StrInTheConcObservat = 'In the concentration observation for %0:s the ' +
  'weight assigned to layer %1:d is zero.';
  StrConcObservationLay = 'Concentration Observation Layer Weight = 0';
  StrInTheConcObservatMult = 'In the concentration observation for %0:s a ' +
  'weight was assigned for layer %1:d but that layer is not part of the ' +
  'multilayer observation.';
  StrConcObservationLayAssigned = 'Concentration Observation Layer Weight ' +
  'incorrectly assigned';
  StrTheFollowingMASSF = 'The following MASS Flux observation names may be v' +
  'alid for MT3DMS or MT3D-USGS but they are not valid for UCODE.';
  StrMT3DMSMassFluxObs = 'MT3DMS or MT3D-USGS Mass Flux Observation';
  StrWritingTOBPackage = 'Writing TOB Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet6 = '  Writing Data Set 6.';
  StrWritingDataSets7to9 = '  Writing Data Sets 7 to 9.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
  StrWritingDataSet4and5 = '  Writing Data Sets 4 and 5.';
  Str0sDefinedByObje = '%0:s defined by object %1:s';


{ TMt3dmsTobWriter }

constructor TMt3dmsTobWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FObservations := TList.Create;
  FFluxObsList := TObsList.Create;
  FArrayWritingFormat := awfMt3dms;
end;

destructor TMt3dmsTobWriter.Destroy;
begin
  FFluxObsList.Free;
  FObservations.Free;
  inherited;
end;

procedure TMt3dmsTobWriter.WriteDataSet7(ObsGroup: TMt3dmsFluxObservationGroup);
var
  iSSType: Integer;
  nFluxTimeObs: Integer;
  ncells: Integer;
begin
  nFluxTimeObs := ObsGroup.ObservationTimes.Count;
  ncells := ObsGroup.NumberOfCells(Model);
  iSSType := FluxObsTypeTo_issType(ObsGroup.FluxObsType);
  // Write Data set 7
  WriteInteger(nFluxTimeObs);
  WriteInteger(ncells);
  WriteInteger(iSSType);
  WriteString(' # Data Set 7: nFluxTimeObs, ncells, iSSType');
  NewLine;
end;

procedure TMt3dmsTobWriter.WriteDataSet8(ObsGroup: TMt3dmsFluxObservationGroup);
var
  FOBSNAM: string;
  TimeIndex: Integer;
  TimeString: string;
  ObsTime: TMt3dmsFluxObservation;
begin
  // Write data set 8
  for TimeIndex := 0 to ObsGroup.ObservationTimes.Count - 1 do
  begin
    ObsTime := ObsGroup.ObservationTimes[TimeIndex];
    TimeString := ObsTime.ObsNameTimeString;
    FOBSNAM := ObsGroup.ObservationName + '_' + TimeString;
    if not UcodeObsNameOK(FOBSNAM) then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingMASSF, FOBSNAM);
    end;
    WriteFluxObsTime(FOBSNAM, ObsGroup.ObservationTimes[TimeIndex]);
  end;
end;

procedure TMt3dmsTobWriter.WriteDataSet9(ObsGroup: TMt3dmsFluxObservationGroup);
var
  VariableIndex: Integer;
  Observer: TObserver;
  CellList: TCellAssignmentList;
  ObjectIndex: Integer;
  Variables: TList;
  CellIndex: Integer;
  Expression: TExpression;
  DataSets: TList;
  ScreenObject: TScreenObject;
  ACell: TCellAssignment;
  ObsFactor: TObservationFactor;
  FactorFormula: string;
  VariablesUsed: TStringList;
  DataArray: TDataArray;
begin
  for ObjectIndex := 0 to ObsGroup.ObservationFactors.Count - 1 do
  begin
    ObsFactor := ObsGroup.ObservationFactors[ObjectIndex];
    FactorFormula := ObsFactor.Factor;
    Model.rpThreeDFormulaCompiler.Compile(FactorFormula);
    Expression := Model.rpThreeDFormulaCompiler.CurrentExpression;
    Assert(Expression.ResultType in [rdtDouble, rdtInteger]);
    DataSets := TList.Create;
    Variables := TList.Create;
    try
      VariablesUsed := Expression.VariablesUsed;
      Variables.Capacity := VariablesUsed.Count;
      DataSets.Capacity := VariablesUsed.Count;
      for VariableIndex := 0 to VariablesUsed.Count - 1 do
      begin
        Observer := Model.GetObserverByName(VariablesUsed[VariableIndex]);
        if Observer is TDataArray then
        begin
          DataArray := TDataArray(Observer);
          DataArray.Initialize;
          Variables.Add(VariablesUsed.Objects[VariableIndex]);
          DataSets.Add(DataArray);
          Model.DataArrayManager.AddDataSetToCache(DataArray);
        end
        else
        begin
          Assert(Observer is TGlobalVariable);
        end;
      end;
      ScreenObject := ObsFactor.ScreenObject as TScreenObject;
      CellList := TCellAssignmentList.Create;
      try
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          WriteFluxObsCell(DataSets, ACell, Expression, Variables, ObsFactor);
        end;
      finally
        CellList.Free;
      end;
    finally
      Variables.Free;
      DataSets.Free;
      Model.DataArrayManager.CacheDataArrays;
    end;
  end;
end;

procedure TMt3dmsTobWriter.WriteFile(const AFileName: string; Purpose: TObservationPurpose);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrTOB) then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingMASSF);

    FNameOfFile := FileName(AFileName);
    WriteToMt3dMsNameFile(StrTOB, Mt3dTob,
      FNameOfFile, foInput, Model);

    FTobPackage := Package as TMt3dmsTransportObservations;

    OUTNAM := ChangeFileExt(ExtractFileName(FNameOfFile), '');

    FillFluxObsList;
    EvaluateConcentrationObs(Purpose);

    FInputFileName := FNameOfFile;
    OpenFile(FNameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingTOBPackage);
      frmProgressMM.AddMessage(StrWritingDataSet0);
      WriteDataSet0;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet1);
      WriteDataSet1;
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

      WriteDataSets3to5;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteFluxObservations;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TMt3dmsTobWriter.FillFluxObsList;
var
  Index: Integer;
begin
  for Index := 0 to Model.Mt3dmsHeadMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsHeadMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsWellMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsWellMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsDrnMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsDrnMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsRivMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsRivMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsGhbMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsGhbMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsRchMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsRchMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsEvtMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsEvtMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsMassLoadingMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsMassLoadingMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsResMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsResMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsLakMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsLakMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsDrtMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsDrtMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsEtsMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsEtsMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsStrMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsStrMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsFhbHeadMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsFhbHeadMassFluxObservations.Items[Index]);
  end;
  for Index := 0 to Model.Mt3dmsFhbFlowMassFluxObservations.Count - 1 do
  begin
    FFluxObsList.Add(Model.Mt3dmsFhbFlowMassFluxObservations.Items[Index]);
  end;
end;

function TMt3dmsTobWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.Mt3dmsTransObs;
end;

procedure TMt3dmsTobWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists; ParameterIndicies: TByteSet;
  Purpose: TObservationPurpose);
var
  DataArrayList: TList;
  TimeListIndex: Integer;
  DisplayTimeList: TMt3dmsTobDisplayTimeList;
  TimeIndex: Integer;
//  CellList: TList;
  DataArray: TModflowBoundaryDisplayDataArray;
  ObsIndex: Integer;
  Obs: TMt3dmsTransObservations;
  ItemIndex: Integer;
  CellList: TMt3dmsTobsCellList;
  Cell: TMt3dmsTob_Cell;
  TimePosition: Integer;
  CellIndex: Integer;
begin
  // Quit if the package isn't used.
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrConcentratonObservationsError);
    if not Package.IsSelected then
    begin
      UpdateNotUsedDisplay(TimeLists);
      Exit;
    end;
    try
      DataArrayList := TList.Create;
      try
        // evaluate all the data used in the package.
        FillFluxObsList;
        EvaluateConcentrationObs(Purpose);
  //      Evaluate(Purpose);

        Assert(TimeLists.Count= 1);
        DisplayTimeList := TimeLists[0] as TMt3dmsTobDisplayTimeList;
        for ObsIndex := 0 to FObservations.Count - 1 do
        begin
          Obs := FObservations[ObsIndex];
          for ItemIndex := 0 to Obs.Values.ObservationConcentrations[Model].Count - 1 do
          begin
            CellList := Obs.Values.ObservationConcentrations[Model][ItemIndex];
            if CellList.Count > 0 then
            begin
              Cell := CellList[0];
              TimePosition := DisplayTimeList.FirstTimeGreaterThan(Cell.Time);
              if TimePosition >= DisplayTimeList.Count then
              begin
                TimePosition := DisplayTimeList.Count-1;
              end;
              for TimeIndex := TimePosition downto Max(0, TimePosition-1) do
              begin
                if DisplayTimeList.Times[TimeIndex] = Cell.Time then
                begin
                  TimePosition := TimeIndex;
                  break;
                end;
              end;
              DataArray := DisplayTimeList[TimePosition]
                as TModflowBoundaryDisplayDataArray;
              for CellIndex := 0 to CellList.Count - 1 do
              begin
                Cell := CellList[CellIndex];
                DataArray.AddDataValue(Cell.ConcentrationAnnotation, Cell.Concentration,
                  Cell.Column, Cell.Row, Cell.Layer);
              end;
            end;
          end;
        end;
        // Mark all the data arrays and time lists as up to date.
        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          DisplayTimeList := TimeLists[TimeListIndex] as TMt3dmsTobDisplayTimeList;
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
    except on E: EInvalidTime do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TMt3dmsTobWriter.WriteFluxObsCell(DataSets: TList;
  ACell: TCellAssignment; Expression: TExpression;
  Variables: TList;
  ObsFactor: TObservationFactor);
var
  TempFormula: string;
  IntegerVariable: TIntegerVariable;
  Compiler: TRbwParser;
  Factor: Double;
  StringVariable: TStringVariable;
  Local_VariableIndex: Integer;
  RealVariable: TRealVariable;
  Layer: Integer;
  Row: Integer;
  BooleanVariable: TBooleanVariable;
  DA_Layer: Integer;
  DA_Row: Integer;
  Column: Integer;
  DA_Column: Integer;
  ScreenObject: TScreenObject;
  DataArray: TDataArray;
begin
  Layer := Model.DataSetLayerToModflowLayer(ACell.Layer);
  Row := ACell.Row + 1;
  Column := ACell.Column + 1;
  for Local_VariableIndex := 0 to Variables.Count - 1 do
  begin
    DataArray := DataSets[Local_VariableIndex];
    DA_Layer := ACell.Layer;
    DA_Row := ACell.Row;
    DA_Column := ACell.Column;
    case DataArray.Orientation of
      dsoTop:
        DA_Layer := 0;
      dsoFront:
        DA_Row := 0;
      dsoSide:
        DA_Column := 0;
      dso3D:
        ;
    else
      // do nothing
      Assert(False);
    end;
    //    DataArray.Initialize;
    case DataArray.DataType of
      rdtDouble:
        begin
          RealVariable := Variables[Local_VariableIndex];
          RealVariable.Value := DataArray.RealData[DA_Layer, DA_Row, DA_Column];
        end;
      rdtInteger:
        begin
          IntegerVariable := Variables[Local_VariableIndex];
          IntegerVariable.Value := DataArray.IntegerData[DA_Layer, DA_Row, DA_Column];
        end;
      rdtBoolean:
        begin
          BooleanVariable := Variables[Local_VariableIndex];
          BooleanVariable.Value := DataArray.BooleanData[DA_Layer, DA_Row, DA_Column];
        end;
      rdtString:
        begin
          StringVariable := Variables[Local_VariableIndex];
          StringVariable.Value := DataArray.StringData[DA_Layer, DA_Row, DA_Column];
        end;
    else
      Assert(False);
    end;
  end;
  UpdateCurrentScreenObject(ObsFactor.ScreenObject as TScreenObject);
  UpdateCurrentSection(ACell.Section);
  try
    Expression.Evaluate;
  except
    on E: ERbwParserError do
    begin
      ScreenObject := ObsFactor.ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(ScreenObject.Name,
        Format(StrObservationFactor, [StrMT3DMSMassFluxObs]), Expression.Decompile, E.Message);
      ObsFactor.Factor := '1.';
      Compiler := Model.rpThreeDFormulaCompiler;
      TempFormula := ObsFactor.Factor;
      Compiler.Compile(TempFormula);
      Expression := Compiler.CurrentExpression;
      Expression.Evaluate;
    end;
  end;
  Factor := Expression.DoubleResult;
  if Factor > 1 then
  begin
    Factor := 1;
  end
  else if Factor < 0 then
  begin
    Factor := 0;
  end;
  WriteInteger(Layer);
  WriteInteger(Row);
  WriteInteger(Column);
  WriteFloat(Factor);
  WriteString(' # Data set 9: kcell, icell, jcell, factor');
  NewLine;
end;

procedure TMt3dmsTobWriter.EvaluateConcentrationObs(Purpose: TObservationPurpose);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Observations: TMt3dmsTransObservations;
  ErrorMessage: string;
  CellList: TMt3dmsTobsCellList;
  ObservationTimeCount: Integer;
  WrongObservationTypesDefined: Boolean;
  ObsIndex: Integer;
  Item: TMt3dmsTobItem;
begin
  nConcObs := 0;
  WrongObservationTypesDefined := False;
  FStartTime := Model.ModflowFullStressPeriods[0].StartTime;
  FEndTime := Model.ModflowFullStressPeriods[
    Model.ModflowFullStressPeriods.Count-1].EndTime;

  frmErrorsAndWarnings.RemoveWarningGroup(Model, ConcOffGrid);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoConcentrationObs);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, MissingConcObsNameError);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, ConcObsNameWarning);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheFollowingObj);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, InvalidConcEndObsTime);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, InvalidConcStartObsTime);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, Mt3dTobErrorRoot);

  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Observations := ScreenObject.Mt3dmsTransObservations;
    if (Observations <> nil) and Observations.Used then
    begin
      if Observations.Purpose = Purpose then
      begin
        Observations.EvaluateConcentrationObservations(Purpose, Model);
        FObservations.Add(Observations);

        if Observations.CellListCount = 0 then
        begin
          ErrorMessage := Format(StrObjectS, [ScreenObject.Name]);
          frmErrorsAndWarnings.AddWarning(Model, ConcOffGrid, ErrorMessage,
            ScreenObject);
          Continue;
        end;

        CellList := Observations.CellLists[0];
        if CellList.Count = 0 then
        begin
          ErrorMessage := Format(StrObjectS, [ScreenObject.Name]);
          frmErrorsAndWarnings.AddWarning(Model, ConcOffGrid, ErrorMessage,
            ScreenObject);
          Continue;
        end;

        ObservationTimeCount := Observations.Values.
          CountObservationTimes(FStartTime, FEndTime);
        nConcObs := nConcObs + ObservationTimeCount;

        if ObservationTimeCount <> Observations.Values.Count then
        begin
          for ObsIndex := 0 to Observations.Values.Count - 1 do
          begin
            Item := Observations.Values.TobItems[ObsIndex];
            if (Item.Time > FEndTime) and (FEvaluationType = etExport) then
            begin
              ErrorMessage := Format(StrObjectSTimeG,
                [ScreenObject.Name, Item.Time]);
              frmErrorsAndWarnings.AddError(Model,
                InvalidConcEndObsTime, ErrorMessage, ScreenObject);
            end;
            if (Item.Time < FStartTime) and (FEvaluationType = etExport) then
            begin
              ErrorMessage := Format(StrObjectSTimeG,
                [ScreenObject.Name, Item.Time]);
              frmErrorsAndWarnings.AddError(Model,
                InvalidConcStartObsTime, ErrorMessage, ScreenObject);
            end;
          end;
        end
      end
      else
      begin
        WrongObservationTypesDefined := True;
      end;
    end;
  end;
  if nConcObs = 0 then
  begin
    if WrongObservationTypesDefined then
    begin
      if (Purpose = ofObserved) then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoConcentrationObs,
          StrNoValidConcObserv);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoConcentrationObs,
          StrNoValidConcPred);
      end;
    end
    else
    begin
      if (Purpose = ofObserved) then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoConcentrationObs,
          StrNoValidConcObs);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoConcentrationObs,
          StrNoValidConcPredForCurrent);
      end;
    end;
  end;

end;

class function TMt3dmsTobWriter.Extension: string;
begin
  Result := '.tob';
end;

procedure TMt3dmsTobWriter.WriteDataSet1;
var
  Index: Integer;
  Obs: TMt3dmsFluxObservationGroup;
  MaxFluxCells: Integer;
begin
  FMaxConcObs := nConcObs;
  FMaxFluxObs := 0;
  MaxFluxCells := 0;
  for Index := 0 to FFluxObsList.Count - 1 do
  begin
    Obs := FFluxObsList[Index];
    MaxFluxCells := MaxFluxCells + Obs.NumberOfCells(Model);
    FMaxFluxObs := FMaxFluxObs + Obs.ObservationTimes.Count;
  end;
  WriteInteger(FMaxConcObs);
  WriteInteger(FMaxFluxObs);
  WriteInteger(MaxFluxCells);
  WriteString(' # Data Set 1: MaxConcObs, MaxFluxObs, MaxFluxCells');
  NewLine;
end;

procedure TMt3dmsTobWriter.WriteDataSet2;
var
  inSaveObs: integer;
  BaseName: string;
begin
  if FTobPackage.SaveBinary = sbSave then
  begin
    inSaveObs := Mt3dTob_inSaveObs;
  end
  else
  begin
    inSaveObs := 0;
  end;
  if FMaxConcObs > 0 then
  begin
    inConcObs := Mt3dTob_inConcObs;
  end
  else
  begin
    inConcObs := 0;
  end;
  if FMaxFluxObs > 0 then
  begin
    inFluxObs := Mt3dTob_inFluxObs;
  end
  else
  begin
    inFluxObs := 0;
  end;
  WriteString(OUTNAM);
  WriteInteger(inConcObs);
  WriteInteger(inFluxObs);
  WriteInteger(inSaveObs);
  WriteString(' # Data Set 2: OUTNAM, inConcObs, inFluxObs, inSaveObs');
  NewLine;

  BaseName := ChangeFileExt(FNameOfFile, '');
  if inConcObs > 0 then
  begin
    WriteToMt3dMsNameFile(StrData, Mt3dTob_inConcObs, ChangeFileExt(BaseName, '.ocn'), foOutput, Model);
  end;
  if inFluxObs > 0 then
  begin
    WriteToMt3dMsNameFile(StrData, Mt3dTob_inFluxObs, ChangeFileExt(BaseName, '.mfx'), foOutput, Model);
  end;
  if inSaveObs > 0 then
  begin
    WriteToMt3dMsNameFile(StrDataBinary, Mt3dTob_inSaveObs, ChangeFileExt(BaseName, '.pst'), foOutput, Model);
  end;

end;

procedure TMt3dmsTobWriter.WriteDataSet4(Observations: TMt3dmsTransObservations;
  CellList: TMt3dmsTobsCellList);
var
  ROFF: Double;
  COFF: Double;
  COBSNAM: string;
  LAYER: Integer;
  ROW: Integer;
  COLUMN: Integer;
  TimeObs: Double;
  ScreenObject: TScreenObject;
  Cell: TMt3dmsTob_Cell;
  TimeIndex: Integer;
  iComp: Integer;
  COBS: Double;
  weight: Double;
  ObsFreq: Integer;
begin
  if CellList.Count = 0 then
  begin
    Exit;
  end;
  Cell := CellList[0];

  COBSNAM := Observations.ObservationName;
  if COBSNAM = '' then
  begin
    ScreenObject := Observations.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      MissingConcObsNameError, ScreenObject.Name, ScreenObject);
  end;
  if not UcodeObsNameOK(COBSNAM) then
  begin
    ScreenObject := Observations.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddWarning(Model, ConcObsNameWarning,
      Format(Str0sDefinedByObje, [COBSNAM, ScreenObject.Name]), ScreenObject);
  end;
  if CellList.Count > 1 then
  begin
    LAYER := -CellList.Count;
  end
  else
  begin
    LAYER := Model.
      DataSetLayerToModflowLayer(Cell.Layer);
  end;
  ROW := Cell.Row+1;
  COLUMN := Cell.Column+1;
  ROFF := Observations.Values.ObservationRowOffset;
  COFF := Observations.Values.ObservationColumnOffset;

  for TimeIndex := 0 to Observations.Values.Count - 1 do
  begin
    CellList := Observations.CellLists[TimeIndex];
    if CellList.Count = 0 then
    begin
      // This is an error but it was reported earlier in
      // TMt3dmsConcObsTimeList.Initialize
      Continue;
    end;
    Cell := CellList[0];
    COBS := Cell.Concentration;
    weight := Cell.Weight;
    iComp := Cell.ComponentIndex + 1;
    TimeObs := 0;
    ObsFreq := 0;
    case Cell.ObsType of
      otTime:
        begin
          TimeObs := Cell.Time - FStartTime;
        end;
      otFrequency:
        begin
          ObsFreq := -Cell.ObsFreq;
        end;
      else
        Assert(False);
    end;
    if Observations.Values.Count > 1 then
    begin
      COBSNAM := Observations.ObservationName + '_' + IntToStr(TimeIndex + 1);
    end;
    WriteString(COBSNAM);
    WriteInteger(LAYER);
    WriteInteger(ROW);
    WriteInteger(COLUMN);
    WriteInteger(iComp);
    case Cell.ObsType of
      otTime: WriteFloat(TimeObs);
      otFrequency: WriteInteger(ObsFreq);
    end;
    WriteFloat(ROFF);
    WriteFloat(COFF);
    WriteFloat(weight);
    WriteFloat(COBS);
    WriteString(' # Data Set 4: COBSNAM, Layer, Row, Column, iComp, '
      + 'TimeObs, Roff, Coff, weight, COBS');
    NewLine;
    if Layer < 0 then
    begin
      WriteDataSet5(Observations, CellList);
    end;
  end;

//
//
//  Item := nil;
//  if Observations.Values.Count > 0 then
//  begin
//    Item := Observations.Values.HobItems[0];
//  end;
//  if (Item <> nil) and (Item.Comment <> '') and (LAYER > 0) then
//  begin
//    WriteString(' Comment = ' + Item.Comment);
//  end;
//  NewLine;
//  if Layer < 0 then
//  begin
//    WriteDataSet5(Observations, CellList);
//  end;
end;

procedure TMt3dmsTobWriter.WriteDataSet5(Observations: TMt3dmsTransObservations;
  CellList: TMt3dmsTobsCellList);
var
  Total: Double;
  ProportionIndex: Integer;
  SortIndex: Integer;
  LayerSort: TLayerSort;
  CellIndex: Integer;
  LayerSorter: TList;
  ActiveDataArray: TDataArray;
  DeltaCol: Integer;
  DeltaRow: Integer;
  WarningMessage: string;
  Cell: TMt3dmsTob_Cell;
  Item: TMultiHeadItem;
begin
  if CellList.Count > 1 then
  begin
    // When the absolute value of ROFF or COFF is less than 0.001,
    // MODFLOW-2000 and MODFLOW-2005 convert them to 0.
    if Observations.Values.ObservationRowOffset < -0.001 then
    begin
      DeltaRow := -1;
    end
    else if Observations.Values.ObservationRowOffset > 0.001 then
    begin
      DeltaRow := 1;
    end
    else
    begin
      DeltaRow := 0;
    end;
    if Observations.Values.ObservationColumnOffset < -0.001 then
    begin
      DeltaCol := -1;
    end
    else if Observations.Values.ObservationColumnOffset > 0.001 then
    begin
      DeltaCol := 1;
    end
    else
    begin
      DeltaCol := 0;
    end;
    Cell := CellList[0];

    if (Cell.Row = 0) and (DeltaRow < 0) then
    begin
      DeltaRow := 0;
    end;
    if (Cell.Row = Model.Grid.RowCount - 1)
      and (DeltaRow > 0) then
    begin
      DeltaRow := 0;
    end;
    if (Cell.Column = 0) and (DeltaCol < 0) then
    begin
      DeltaCol := 0;
    end;
    if (Cell.Column = Model.Grid.ColumnCount - 1)
      and (DeltaCol > 0) then
    begin
      DeltaCol := 0;
    end;
    ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    Assert(ActiveDataArray <> nil);
    ActiveDataArray.Initialize;
    LayerSorter := TObjectList.Create;
    try
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        Cell := CellList[CellIndex];
        LayerSort := TLayerSort.Create;
        LayerSorter.Add(LayerSort);
        LayerSort.Layer := Cell.Layer;
        LayerSort.ActiveCells := 1;
        if not ActiveDataArray.BooleanData[Cell.Layer,
          Cell.Row, Cell.Column] then
        begin
          LayerSort.ActiveCells := -3;
        end;
        if DeltaRow <> 0 then
        begin
          if ActiveDataArray.BooleanData[Cell.Layer,
            Cell.Row + DeltaRow, Cell.Column] then
          begin
            LayerSort.ActiveCells := LayerSort.ActiveCells + 1;
          end;
        end;
        if DeltaCol <> 0 then
        begin
          if ActiveDataArray.BooleanData[Cell.Layer,
            Cell.Row, Cell.Column + DeltaCol] then
          begin
            LayerSort.ActiveCells := LayerSort.ActiveCells + 1;
          end;
        end;
        if (DeltaRow <> 0) and (DeltaCol <> 0) then
        begin
          if ActiveDataArray.BooleanData[Cell.Layer,
            Cell.Row + DeltaRow, Cell.Column + DeltaCol] then
          begin
            LayerSort.ActiveCells := LayerSort.ActiveCells + 1;
          end;
        end;
      end;
      for ProportionIndex := 0 to
        Observations.LayerFractions.Count - 1 do
      begin
        Item := Observations.LayerFractions[ProportionIndex];
        Item.Used := False;
      end;
      LayerSorter.Sort(SortLayerSorts);
      for SortIndex := 0 to LayerSorter.Count - 1 do
      begin
        LayerSort := LayerSorter[SortIndex];
        LayerSort.Proportion := 0;
        if LayerSort.ActiveCells > 0 then
        begin
          for ProportionIndex := 0 to
            Observations.LayerFractions.Count - 1 do
          begin
            Item := Observations.LayerFractions[ProportionIndex];
            if LayerSort.Layer+1 = Item.Layer then
            begin
              LayerSort.Proportion := Item.Proportion;
              Item.Used := True;
              break;
            end;
          end;
        end;
      end;
      Total := 0;
      for SortIndex := 0 to LayerSorter.Count - 1 do
      begin
        LayerSort := LayerSorter[SortIndex];
        Total := Total + LayerSort.Proportion;
      end;
      if Total > 0 then
      begin
        for SortIndex := 0 to LayerSorter.Count - 1 do
        begin
          LayerSort := LayerSorter[SortIndex];
          LayerSort.Proportion := LayerSort.Proportion / Total;
        end;
      end
      else
      begin
        for SortIndex := 0 to LayerSorter.Count - 1 do
        begin
          LayerSort := LayerSorter[SortIndex];
          LayerSort.Proportion := 1 / LayerSorter.Count;
        end;
      end;
      for SortIndex := 0 to LayerSorter.Count - 1 do
      begin
        LayerSort := LayerSorter[SortIndex];

        if LayerSort.Proportion = 0 then
        begin
          WarningMessage := Format(StrInTheConcObservat,
            [(Observations.ScreenObject as TScreenObject).Name,
            LayerSort.Layer+1]);
          frmErrorsAndWarnings.AddWarning(Model, StrConcObservationLay,
            WarningMessage, Observations.ScreenObject);
        end;

        WriteInteger(LayerSort.Layer+1);
        WriteFloat(LayerSort.Proportion);
        if (((SortIndex + 1) mod 10) = 0)
          or (SortIndex = LayerSorter.Count - 1) then
        begin
          if (SortIndex = LayerSorter.Count - 1) then
          begin
            WriteString(' # Data Set 5: mLayer(1), prLayer(1), mLayer(2), '
              + 'prLayer(2), ..., mLayer(|Layer|), prLayer(|Layer|)');
          end;
          NewLine;
        end;
      end;
      for ProportionIndex := 0 to
        Observations.LayerFractions.Count - 1 do
      begin
        Item := Observations.LayerFractions[ProportionIndex];
        if not Item.Used then
        begin
          WarningMessage := Format(StrInTheConcObservatMult,
            [(Observations.ScreenObject as TScreenObject).Name, Item.Layer]);
          frmErrorsAndWarnings.AddWarning(Model, StrConcObservationLayAssigned,
            WarningMessage, Observations.ScreenObject);
        end;
      end;
    finally
      LayerSorter.Free;
    end;
  end;
end;

procedure TMt3dmsTobWriter.WriteFluxObsTime(const FOBSNAM: string; TimeItem: TMt3dmsFluxObservation);
var
  iComp: integer;
  FluxTimeObs: double;
  weight_fobs: double;
  FluxObs: double;
  FluxFreq: Integer;
begin
  iComp := Model.IndexOfMt3dmsSpeciesName(TimeItem.Species) + 1;

  FluxTimeObs := 0;
  FluxFreq := 0;
  case TimeItem.ObservationType of
    otTime:
      begin
        FluxTimeObs := TimeItem.Time - FStartTime;
      end;
    otFrequency:
      begin
        FluxFreq := -TimeItem.ObservationFrequency;
      end
    else
      Assert(False);
  end;

  weight_fobs := TimeItem.Weight;
  FluxObs := TimeItem.ObservedValue;
  WriteString(FOBSNAM);
  WriteInteger(iComp);
  case TimeItem.ObservationType of
    otTime:
      begin
        WriteFloat(FluxTimeObs);
      end;
    otFrequency:
      begin
        WriteInteger(FluxFreq);
      end
    else
      Assert(False);
  end;
  WriteFloat(weight_fobs);
  WriteFloat(FluxObs);
  WriteString(' # Data set 8: FOBSNAM, iComp, FluxTimeObs, weight_fobs, FluxObs');
  NewLine;
end;

procedure TMt3dmsTobWriter.WriteDatSets4and5(Index: integer);
var
  Observations: TMt3dmsTransObservations;
  CellList: TMt3dmsTobsCellList;
begin
  Observations := FObservations[Index];
  if Observations.CellListCount > 0 then
  begin
    CellList := Observations.CellLists[0];
    if CellList.Count > 0 then
    begin
      WriteDataSet4(Observations, CellList);
    end;
  end;
end;

procedure TMt3dmsTobWriter.WriteFluxObservations;
var
  Index: Integer;
  nFluxGroup: Integer;
  FScale: Double;
  iOutFlux: Integer;
begin
  if inFluxObs <= 0 then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet6);
  // write data set 6
  nFluxGroup := FFluxObsList.Count;
  FScale := FTobPackage.FluxScaleFactor;
  iOutFlux := Ord(FTobPackage.MassFluxObsResult);
  WriteInteger(nFluxGroup);
  WriteFloat(FScale);
  WriteInteger(iOutFlux);
  WriteString(' # Data Set 6: nFluxGroup, FScale, iOutFlux');
  NewLine;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrWritingDataSets7to9);
  for Index := 0 to FFluxObsList.Count - 1 do
  begin
    // Write data sets 7, 8, and 9
    WriteDataSets7to9(FFluxObsList[Index]);
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  end;
end;

procedure TMt3dmsTobWriter.WriteDataSets3to5;
var
  Index: Integer;
  CScale: Double;
  iOutCobs: Integer;
  iConcLOG: Integer;
  iConcINTP: Integer;
begin
  if inConcObs <= 0 then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet3);
  CScale := FTobPackage.ConcScaleFactor;
  iOutCobs := Ord(FTobPackage.ConcObsResult);
  iConcLOG := Ord(FTobPackage.TransformType);
  iConcINTP := Ord(FTobPackage.InterpolateObs);
  // Data set 3
  WriteInteger(nConcObs);
  WriteFloat(CScale);
  WriteInteger(iOutCobs);
  WriteInteger(iConcLOG);
  WriteInteger(iConcINTP);
  WriteString(' # Data Set 3: nConcObs, CScale, iOutCobs, iConcLOG, iConcINTP');
  NewLine;

  frmProgressMM.AddMessage(StrWritingDataSet4and5);
  for Index := 0 to FObservations.Count - 1 do
  begin
    WriteDatSets4and5(Index);
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  end;

end;

procedure TMt3dmsTobWriter.WriteDataSets7to9(
  ObsGroup: TMt3dmsFluxObservationGroup);
begin
  WriteDataSet7(ObsGroup);
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  WriteDataSet8(ObsGroup);
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  WriteDataSet9(ObsGroup);
end;

end.
