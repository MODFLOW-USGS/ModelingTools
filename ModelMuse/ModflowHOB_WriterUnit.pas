unit ModflowHOB_WriterUnit;

interface

uses System.UITypes, Winapi.Windows, SysUtils, Math, Classes, Contnrs,
  PhastModelUnit, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, ModflowHobUnit, ModflowBoundaryDisplayUnit,
  GoPhastTypes, RealListUnit, Vcl.Dialogs;

type
  TModflowHobWriter = class(TCustomPackageWriter)
  private
    NH: Integer;
    MOBS: Integer;
    MAXM: Integer;
    IUHOBSV: Integer;
    // @name contains the @link(THobBoundary)s that are used.
    FObservations: TList;
    FStartTime: Double;
    FEndTime: Double;
    IREFSP: Integer;
    FStartingTimes: TRealList;
    FOutFileName: string;
    FPestInstructionFile: TStringList;
    procedure Evaluate(Purpose: TObservationPurpose);
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3to6(Index: integer);
    procedure WriteDataSet3(Observations: THobBoundary; CellList: TObsCellList);
    procedure WriteDataSet4(Observations: THobBoundary; CellList: TObsCellList);
    procedure WriteDataSet5(Observations: THobBoundary);
    procedure WriteDataSet6(Observations: THobBoundary);
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists;
      ParameterIndicies: TByteSet; Purpose: TObservationPurpose);
    procedure WriteFile(const AFileName: string; Purpose: TObservationPurpose);
  end;

implementation

uses ModflowUnitNumbers, ScreenObjectUnit, DataSetUnit,
  frmErrorsAndWarningsUnit, frmProgressUnit, Forms, ModflowTimeUnit,
  ModflowBoundaryUnit;

resourcestring
  ObsNameWarning = 'The following Head observation names may be valid for MODFLOW but they are not valid for UCODE.';
  ObsNameWarningPest = 'The following Head observation names may be valid for MODFLOW but they are not valid for PEST.';
  MissingObsNameError = 'The head observation in the following objects do not have observations names assigned';
  HeadOffGrid = 'One or more head observation are not located on the grid and will be ignored';
  NoHeads = 'No head observations';
  StrNoValidHeadObserv = 'No valid head observations were defined. Check tha' +
  't "Model|Observation Type" is set to the correct value and that the obser' +
  'vation type for each observation is set correctly.';
  StrNoValidHeadPred = 'No valid head observations were defined for the curr' +
  'ent stress period. Check that "Model|Observation Type" is set to the corr' +
  'ect value and that the observation type for each observation is set corre' +
  'ctly.';
  StrNoValidHeadObs = 'No valid head observations were defined ' +
    'for the current stress period.';
  StrNoValidHeadPredForCurrent = 'No valid head predictions were defined ' +
  'for the current stress period.';
  InvalidEndObsTime = 'Head observation time after end of simulation';
  InvalidStartObsTime = 'Head observation time before beginning of simulation';
  StrHeadObservationLay = 'Head Observation Layer Weight = 0';
  StrInTheHeadObservat = 'In the head observation for %0:s the weight assign' +
  'ed to layer %1:d is zero.';
  StrInTheHeadObservatMult = 'In the head observation for %0:s a weight was assi' +
  'gned for layer %1:d but that layer is not part of the multilayer observat' +
  'ion.';
  StrHeadObservationLayAssigned = 'Head Observation Layer Weight incorrectly assigne' +
  'd';
  Str0sDefinedByObje = '%0:s defined by object %1:s';
  StrWritingHOBPackage = 'Writing HOB Package input.';
  StrEvaluatingData = 'Evaluating data.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
  StrWritingDataSets3to6 = '  Writing Data Sets 3 to 6.';
  StrCheckThatAnyObjec = 'Check that any objects that define head observatio' +
  'ns are in active cells.';
  StrCheckThatTheObser = 'Check that the observation times are between the b' +
  'eginning of the first stress period and the end of the last stress period' +
  '.';
  StrInvalidHeadObserva = 'Invalid Head Observation layer weight';
  StrUnusedLayerFraction = 'In the head observation for %0:s the weight assi' +
  'gned to layer %1:d is not used because that layer is not part of the obse' +
  'rvation. The layers that are part of the observation are determined by th' +
  'e Z coordinates for the object.';

{ TModflowHobWriter }

constructor TModflowHobWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FObservations := TList.Create;
  FPestInstructionFile := TStringList.Create;
  FPestInstructionFile.Add('pif @');
  FPestInstructionFile.Add('l1');
end;

destructor TModflowHobWriter.Destroy;
begin
  FPestInstructionFile.Free;
  FObservations.Free;
  inherited;
end;

procedure TModflowHobWriter.Evaluate(Purpose: TObservationPurpose);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Observations: THobBoundary;
  CellList: TObsCellList;
  ErrorMessage: string;
  ObsIndex: Integer;
  Item: THobItem;
  ObservationTimeCount: Integer;
  WrongObservationTypesDefined: boolean;
  ObsObjectsDefined: boolean;
begin
  WrongObservationTypesDefined := False;
  FStartTime := Model.ModflowFullStressPeriods[0].StartTime;
  FEndTime := Model.ModflowFullStressPeriods[
    Model.ModflowFullStressPeriods.Count-1].EndTime;
  IUHOBSV := Model.UnitNumbers.UnitNumber(StrIUHOBSV);
  NH := 0;
  MOBS := 0;
  MAXM := 2;

  frmErrorsAndWarnings.RemoveErrorGroup(Model, MissingObsNameError);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, NoHeads);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, InvalidStartObsTime);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, InvalidEndObsTime);

  frmErrorsAndWarnings.RemoveWarningGroup(Model, ObsNameWarning);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, ObsNameWarningPest);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, HeadOffGrid);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, NoHeads);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHeadObservationLay);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHeadObservationLayAssigned);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrOneOrMoreHeadObs);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidHeadObserva);


  ObsObjectsDefined := False;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Observations := ScreenObject.ModflowHeadObservations;
    if (Observations <> nil) and Observations.Used then
    begin
      ObsObjectsDefined := True;
      if Observations.Purpose = Purpose then
      begin
        Observations.EvaluateHeadObservations(Purpose, Model);
        FObservations.Add(Observations);

        if Observations.CellListCount = 0 then
        begin
          ErrorMessage := Format(StrObjectS, [ScreenObject.Name]);
          frmErrorsAndWarnings.AddWarning(Model, HeadOffGrid, ErrorMessage, ScreenObject);
          Continue;
        end;

        CellList := Observations.CellLists[0];
        if CellList.Count = 0 then
        begin
          ErrorMessage := Format(StrObjectS, [ScreenObject.Name]);
          frmErrorsAndWarnings.AddWarning(Model, HeadOffGrid, ErrorMessage, ScreenObject);
          Continue;
        end;

        ObservationTimeCount := Observations.Values.
          CountObservationTimes(FStartTime, FEndTime);
        NH := NH + ObservationTimeCount;

        if CellList.Count > 1 then
        begin
          Inc(MOBS, Observations.Values.Count);
        end;
        if CellList.Count > MAXM then
        begin
          MAXM := CellList.Count;
        end;
        if ObservationTimeCount <> Observations.Values.Count then
        begin
          for ObsIndex := 0 to Observations.Values.Count - 1 do
          begin
            Item := Observations.Values.HobItems[ObsIndex];
            if (Item.Time > FEndTime) and (FEvaluationType = etExport) then
            begin
              ErrorMessage := Format(StrObjectSTimeG,
                [ScreenObject.Name, Item.Time]);
//              ErrorMessage := 'Object: ' + ScreenObject.Name
//                + '; Time: ' + FloatToStr(Item.Time);
              frmErrorsAndWarnings.AddError(Model,
                InvalidEndObsTime, ErrorMessage, ScreenObject);
            end;
            if (Item.Time < FStartTime) and (FEvaluationType = etExport) then
            begin
              ErrorMessage := Format(StrObjectSTimeG,
                [ScreenObject.Name, Item.Time]);
//              ErrorMessage := 'Object: ' + ScreenObject.Name
//                + '; Time: ' + FloatToStr(Item.Time);
              frmErrorsAndWarnings.AddError(Model,
                InvalidStartObsTime, ErrorMessage, ScreenObject);
            end;
          end;
        end;
      end
      else
      begin
        WrongObservationTypesDefined := True;
      end;
    end;
  end;
  if NH = 0 then
  begin
    if ObsObjectsDefined then
    begin
      frmErrorsAndWarnings.AddError(Model, NoHeads, StrCheckThatAnyObjec);
      frmErrorsAndWarnings.AddError(Model, NoHeads, StrCheckThatTheObser);
    end;
    if WrongObservationTypesDefined then
    begin
      if (FEvaluationType = etExport) then
      begin
        frmErrorsAndWarnings.AddError(Model, NoHeads, StrNoValidHeadObserv);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, NoHeads, StrNoValidHeadPred);
      end;
    end
    else
    begin
      if (FEvaluationType = etExport) then
      begin
        frmErrorsAndWarnings.AddError(Model, NoHeads,
          StrNoValidHeadObs);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, NoHeads,
          StrNoValidHeadPredForCurrent);
      end;
    end;
  end;
end;

class function TModflowHobWriter.Extension: string;
begin
  result := '.ob_hob';
end;

function TModflowHobWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.HobPackage;
end;

procedure TModflowHobWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists; ParameterIndicies: TByteSet;
  Purpose: TObservationPurpose);
var
  DataArrayList: TList;
  TimeListIndex: Integer;
  DisplayTimeList: THobDisplayTimeList;
  TimeIndex: Integer;
//  CellList: TList;
  DataArray: TModflowBoundaryDisplayDataArray;
  ObsIndex: Integer;
  Obs: THobBoundary;
  ItemIndex: Integer;
  CellList: TObsCellList;
  Cell: THob_Cell;
  TimePosition: Integer;
  CellIndex: Integer;
begin
  // Quit if the package isn't used.
  try
    frmErrorsAndWarnings.BeginUpdate;
    try
      frmErrorsAndWarnings.RemoveErrorGroup(Model, StrHeadObservationsError);
      if not Package.IsSelected then
      begin
        UpdateNotUsedDisplay(TimeLists);
        Exit;
      end;
      DataArrayList := TList.Create;
      try
        // evaluate all the data used in the package.
        Evaluate(Purpose);

        Assert(TimeLists.Count= 1);
        DisplayTimeList := TimeLists[0] as THobDisplayTimeList;
        for ObsIndex := 0 to FObservations.Count - 1 do
        begin
          Obs := FObservations[ObsIndex];
          for ItemIndex := 0 to Obs.Values.ObservationHeads[Model].Count - 1 do
          begin
            CellList := Obs.Values.ObservationHeads[Model][ItemIndex];
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
                DataArray.AddDataValue(Cell.HeadAnnotation, Cell.Head,
                  Cell.Column, Cell.Row, Cell.Layer);
              end;
            end;
          end;
        end;
        // Mark all the data arrays and time lists as up to date.
        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          DisplayTimeList := TimeLists[TimeListIndex] as THobDisplayTimeList;
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
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TModflowHobWriter.WriteDataSet1;
var
  HOBDRY: double;
  PrintObservations: Boolean;
begin
  HOBDRY := Model.ModflowPackages.HobPackage.DryHead;
  WriteInteger(NH);
  WriteInteger(MOBS);
  WriteInteger(MAXM);
  WriteInteger(IUHOBSV);
  WriteFloat(HOBDRY);
  PrintObservations := Model.ModflowOutputControl.PrintObservations;
  if not PrintObservations then
  begin
    WriteString(' NOPRINT');
  end;
  WriteString(' # Data Set 1: NH MOBS MAXM IUHOBSV HOBDRY');
  if not PrintObservations then
  begin
    WriteString(' NOPRINT');
  end;
  NewLine;
end;

procedure TModflowHobWriter.WriteDataSet2;
const
  TOMULTH = 1;
begin
  WriteFloat(TOMULTH);
  WriteString(' # Data Set 2: TOMULTH');
  NewLine;
end;

procedure TModflowHobWriter.WriteDataSet3to6(Index: integer);
var
  Observations: THobBoundary;
  CellList: TObsCellList;
begin
  Observations := FObservations[Index];
  if Observations.CellListCount > 0 then
  begin
    CellList := Observations.CellLists[0];
    if CellList.Count > 0 then
    begin
      WriteDataSet3(Observations, CellList);
      WriteDataSet4(Observations, CellList);
      WriteDataSet5(Observations);
      WriteDataSet6(Observations);
    end;
  end;
end;

procedure TModflowHobWriter.WriteFile(const AFileName: string; Purpose: TObservationPurpose);
var
  NameOfFile: string;
  Index: Integer;
  TimeIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrHOB) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrHeadObservationsError);
    frmProgressMM.AddMessage(StrWritingHOBPackage);
    frmProgressMM.AddMessage(StrEvaluatingData);
    Evaluate(Purpose);
    NameOfFile := FileName(AFileName);
    WriteToNameFile(StrHOB, Model.UnitNumbers.UnitNumber(StrHOB), NameOfFile, foInput, Model);
    if IUHOBSV <> 0 then
    begin
      FOutFileName := ChangeFileExt(NameOfFile, StrHobout);
      WriteToNameFile(StrDATA, IUHOBSV, FOutFileName, foOutput, Model);
    end;
    OpenFile(NameOfFile);
    try
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

      frmProgressMM.AddMessage(StrWritingDataSets3to6);
      WriteDataSet2;

      FStartingTimes := TRealList.Create;
      try
        for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
        begin
          FStartingTimes.Add(Model.ModflowFullStressPeriods[TimeIndex].StartTime);
        end;
        FStartingTimes.Sort;

        for Index := 0 to FObservations.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          WriteDataSet3to6(Index);
        end;
      finally
        FStartingTimes.Free;
      end;
    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;

  if Model.PestUsed then
  begin
    FPestInstructionFile.SaveToFile(FOutFileName + '.ins');
  end;
end;

procedure TModflowHobWriter.WriteDataSet6(Observations: THobBoundary);
//const
//  IREFSP = 1;
var
  HOBS: Double;
  TOFFSET: Double;
  Item: THobItem;
  OBSNAM: string;
  ObsIndex: Integer;
  Comment: string;
  ReferenceStressPeriodIndex: Integer;
  IREFSP: integer;
begin
  if Observations.Values.Count > 1 then
  begin
    for ObsIndex := 0 to Observations.Values.Count - 1 do
    begin
      OBSNAM := Observations.ObservationName + '_' + IntToStr(ObsIndex + 1);
      if Length(OBSNAM) > 12 then
      begin
        OBSNAM := Observations.ObservationName + IntToStr(ObsIndex + 1);
      end;
      if Length(OBSNAM) > 12 then
      begin
        // The GUI is designed to prevent this from ever being required.
        SetLength(OBSNAM, 12);
      end;
      Item := Observations.Values.HobItems[ObsIndex];
      if (FStartTime <= Item.Time) and (Item.Time <= FEndTime) then
      begin
        ReferenceStressPeriodIndex := FStartingTimes.IndexOfClosest(Item.Time);
        if (FStartingTimes[ReferenceStressPeriodIndex] > Item.Time) then
        begin
          Dec(ReferenceStressPeriodIndex);
        end;
        Assert(ReferenceStressPeriodIndex >= 0);


        TOFFSET := Item.Time - FStartingTimes[ReferenceStressPeriodIndex];
        HOBS := Item.Head;
        IREFSP := ReferenceStressPeriodIndex+1;

        // If the observation time is at the end of a steady state stress period,
        // make the reference stress period be the steady state stress period
        // rather than the following stress period. This also requires
        // an adjustment of TOFFSET.
        if (ReferenceStressPeriodIndex > 0) and (TOFFSET = 0)
          and (Model.ModflowFullStressPeriods[ReferenceStressPeriodIndex-1].
          StressPeriodType = sptSteadyState) then
        begin
          TOFFSET := Model.ModflowFullStressPeriods[
            ReferenceStressPeriodIndex-1].PeriodLength;
          IREFSP := IREFSP-1;
        end;

        WriteString(OBSNAM);
        WriteInteger(IREFSP);
        WriteFloat(TOFFSET);
        WriteFloat(HOBS);
        WriteString(' # Data Set 6: OBSNAM IREFSP TOFFSET HOBS');
        Comment := Item.Comment;
        if Comment <> '' then
        begin
          WriteString(' Comment = ' + Comment);
        end;
        NewLine;

        FPestInstructionFile.Add(Format('l1 !%s!', [OBSNAM]));
      end;
    end;
  end;
end;

procedure TModflowHobWriter.WriteDataSet5(Observations: THobBoundary);
var
  ITT: Integer;
begin
  if IREFSP < 0 then
  begin
    ITT := Ord(Observations.MultiObsMethod) + 1;
    WriteInteger(ITT);
    WriteString(' # Data Set 5: ITT');
    NewLine;
  end;
end;

procedure TModflowHobWriter.WriteDataSet4(Observations: THobBoundary; CellList: TObsCellList);
var
  Total: Double;
  Item: TMultiHeadItem;
  ProportionIndex: Integer;
  SortIndex: Integer;
  LayerSort: TLayerSort;
  CellIndex: Integer;
  LayerSorter: TList;
  ActiveDataArray: TDataArray;
  Cell: THob_Cell;
  DeltaCol: Integer;
  DeltaRow: Integer;
  WarningMessage: string;
  Layer: integer;
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
        if not Model.IsLayerSimulated(Cell.Layer) then
        begin
          Continue;
        end;
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
      for ProportionIndex := 0 to
        Observations.LayerFractions.Count - 1 do
      begin
        Item := Observations.LayerFractions[ProportionIndex];
        if not Item.Used then
        begin
          WarningMessage := Format(StrUnusedLayerFraction,
            [(Observations.ScreenObject as TScreenObject).Name,
            Item.Layer]);
          frmErrorsAndWarnings.AddWarning(Model, StrInvalidHeadObserva,
            WarningMessage, Observations.ScreenObject);
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
          WarningMessage := Format(StrInTheHeadObservat,
            [(Observations.ScreenObject as TScreenObject).Name,
            LayerSort.Layer+1]);
          frmErrorsAndWarnings.AddWarning(Model, StrHeadObservationLay,
            WarningMessage, Observations.ScreenObject);
        end;

        LAYER := Model.
          DataSetLayerToModflowLayer(LayerSort.Layer);
        WriteInteger(LAYER);
        WriteFloat(LayerSort.Proportion);
        if (((SortIndex + 1) mod 10) = 0)
          or (SortIndex = LayerSorter.Count - 1) then
        begin
          if (SortIndex = LayerSorter.Count - 1) then
          begin
            WriteString(' # Data Set 4: MLAY(1), PR(1), MLAY(2), '
              + 'PR(2), ..., MLAY(|LAYER|), PR(|LAYER|)');
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
          WarningMessage := Format(StrInTheHeadObservatMult,
            [(Observations.ScreenObject as TScreenObject).Name, Item.Layer]);
          frmErrorsAndWarnings.AddWarning(Model, StrHeadObservationLayAssigned,
            WarningMessage, Observations.ScreenObject);
        end;
      end;
    finally
      LayerSorter.Free;
    end;
  end;
end;

procedure TModflowHobWriter.WriteDataSet3(Observations: THobBoundary;
  CellList: TObsCellList);
var
  ROFF: Double;
  COFF: Double;
  HOBS: Double;
  Cell: THob_Cell;
  ACell: THob_Cell;
  OBSNAM: string;
  LAYER: Integer;
  ROW: Integer;
  COLUMN: Integer;
  TOFFSET: Double;
  Item: THobItem;
  ObservationTimeCount: Integer;
  ScreenObject: TScreenObject;
  ReferenceStressPeriodIndex: Integer;
  CellIndex: Integer;
begin
  if CellList.Count = 0 then
  begin
    Exit;
  end;
  Cell := CellList[0];
  ReferenceStressPeriodIndex := FStartingTimes.IndexOfClosest(Cell.Time);
  if (FStartingTimes[ReferenceStressPeriodIndex] > Cell.Time) then
  begin
    Dec(ReferenceStressPeriodIndex);
  end;
  Assert(ReferenceStressPeriodIndex >= 0);

  OBSNAM := Observations.ObservationName;
  if OBSNAM = '' then
  begin
    ScreenObject := Observations.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      MissingObsNameError, ScreenObject.Name, ScreenObject);
  end;
  if Model.PestUsed then
  begin
    if not PestObsNameOK(OBSNAM) then
    begin
      ScreenObject := Observations.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddWarning(Model, ObsNameWarningPest,
        Format(Str0sDefinedByObje, [OBSNAM, ScreenObject.Name]), ScreenObject);
    end;
  end
  else
  begin
    if not UcodeObsNameOK(OBSNAM) then
    begin
      ScreenObject := Observations.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddWarning(Model, ObsNameWarning,
        Format(Str0sDefinedByObje, [OBSNAM, ScreenObject.Name]), ScreenObject);
    end;
  end;
  if CellList.Count > 1 then
  begin
    LAYER := 0;
    for CellIndex := 0 to CellList.Count - 1 do
    begin
      ACell := CellList[CellIndex];
      if Model.IsLayerSimulated(ACell.Layer) then
      begin
        Dec(LAYER);
      end;
    end;
  end
  else
  begin
    LAYER := Model.
      DataSetLayerToModflowLayer(Cell.Layer);
  end;
  ROW := Cell.Row+1;
  COLUMN := Cell.Column+1;
  ObservationTimeCount := Observations.Values.
    CountObservationTimes(FStartTime, FEndTime);
  TOFFSET := Cell.Time - FStartingTimes[ReferenceStressPeriodIndex];
  if ObservationTimeCount = 1 then
  begin
    IREFSP := ReferenceStressPeriodIndex+1;
  end
  else
  begin
    IREFSP := -ObservationTimeCount;
  end;

  if IREFSP > 0 then
  begin
    FPestInstructionFile.Add(Format('l1 !%s!', [OBSNAM]));
  end;

  // If the observation time is at the end of a steady state stress period,
  // make the reference stress period be the steady state stress period
  // rather than the following stress period. This also requires an adjustment of
  // TOFFSET.
  if (ReferenceStressPeriodIndex > 0) and (TOFFSET = 0)
    and (Model.ModflowFullStressPeriods[ReferenceStressPeriodIndex-1].
    StressPeriodType = sptSteadyState) then
  begin
    TOFFSET := Model.ModflowFullStressPeriods[
      ReferenceStressPeriodIndex-1].PeriodLength;
    if ObservationTimeCount = 1 then
    begin
      IREFSP := IREFSP-1;
    end;
  end;


  ROFF := Observations.Values.ObservationRowOffset;
  COFF := Observations.Values.ObservationColumnOffset;
  HOBS := Cell.Head;
  WriteString(OBSNAM);
  WriteInteger(LAYER);
  WriteInteger(ROW);
  WriteInteger(COLUMN);
  WriteInteger(IREFSP);
  WriteFloat(TOFFSET);
  WriteFloat(ROFF);
  WriteFloat(COFF);
  WriteFloat(HOBS);
  WriteString(' # Data Set 3: OBSNAM LAYER ROW COLUMN IREFSP TOFFSET ROFF COFF HOBS');
  Item := nil;
  if Observations.Values.Count > 0 then
  begin
    Item := Observations.Values.HobItems[0];
  end;
  if (Item <> nil) and (Item.Comment <> '') and (LAYER > 0) then
  begin
    WriteString(' Comment = ' + Item.Comment);
  end;
  NewLine;
end;

{ THobDisplayTimeList }

end.
