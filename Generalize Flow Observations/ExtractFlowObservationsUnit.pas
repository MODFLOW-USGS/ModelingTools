unit ExtractFlowObservationsUnit;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, GetVersionUnit,
  System.Generics.Collections, ReadStressPeriodsUnit, FlowObs, RealListUnit,
  ReadFlowObsInput, ReadModflowArrayUnit, CBC_ReaderUnit;

type
  EConsoleError = class(Exception);
  ENameFileError = class(Exception);
  EExtractError = class(Exception);

  TTimeStep = class(TObject)
    // @name starts at 1.
    StressPeriod: integer;
    // @name starts at 1.
    Step: integer;
    // @name is the time from the beginning of the stress period
    // to the end of the time step.
    PerTime: double;
    // @name is the time from the beginning of the simulation
    // to the end of the time step.
    TotalTime: Double;
    StressPeriodType: TStressPeriodType;
  end;

  TTimeStepList = TObjectList<TTimeStep>;

  TFlowExtractor = class(TObject)
  private
    FListingFile: TStreamWriter;
    FLineSplitter: TStringList;
    FListingFileName: string;
    FDisFileName: string;
    FInputFileName: string;
    FOutputFileName: string;
    FFlowFileName: string;
    FTimeSteps: TTimeStepList;
    FStressPeriods: TStressPeriods;
    FFlowLists: TFlowLists;
    FTimes: TRealList;
    FFlowObs: TFlowObservations;
    FEarlierList : TStructuredCellList;
    FLaterList : TStructuredCellList;
    FCBC_Reader: TCBC_Reader;
    NROW: Integer;
    NCOL: Integer;
    NLAY: Integer;
    FOutputFile: TStreamWriter;
    procedure ReadNameFile(const NameFile: string);
    procedure ReadFiles(const NameFile: string);
    procedure AnalyzeTimeSteps;
    procedure SortFlowObs;
    procedure GetObsTimes(FlowList: TFlowList; ObsTimes: TRealList);
    procedure EvaluteSimulatedValues;
  public
    constructor Create(const NameFile: string);
  end;

procedure ExtractFlowObservations;

implementation

uses
  System.Math;

procedure ExtractFlowObservations;
var
  InputFileName: string;
  index: Integer;
begin
  FormatSettings.DecimalSeparator := '.';
  if ParamCount >= 1 then
  begin
    InputFileName := ParamStr(1);
  end
  else
  begin
    for index := 1 to 3 do
    begin
      Writeln('What is the name of the Name File?');
      ReadLn(InputFileName);
      if Length(InputFileName) > 0then
      begin
        if InputFileName[1] = '"' then
        begin
          if InputFileName[Length(InputFileName)] = '"' then
          begin
            InputFileName := Copy(InputFileName, 2, Length(InputFileName)-2);
          end;
        end;
        if FileExists(InputFileName) then
        begin
          Break;
        end
        else
        begin
          WriteLn(Format('%s does not exist.',[InputFileName]));
        end;
      end;
    end;
  end;
  if not FileExists(InputFileName) then
  begin
    raise EConsoleError.Create(Format('%s does not exist.', [InputFileName]));
  end;

  with TFlowExtractor.Create(InputFileName) do
  begin
    Free;
  end;
end;

{ TFlowExtractor }

constructor TFlowExtractor.Create(const NameFile: string);
begin
  FLineSplitter := TStringList.Create;
  try
    FLineSplitter.Delimiter := ' ';
    FLineSplitter.QuoteChar := '"';
    FListingFile := nil;
    try
      try
        ReadFiles(NameFile);
      except on E: Exception do
        begin
          if FListingFile <> nil then
          begin
            FListingFile.WriteLine(E.Message);
          end;
          raise;
        end;
      end;
    finally
      FListingFile.Free;
      FOutputFile.Free;
    end;
  finally
    FLineSplitter.Free;
  end;
end;

procedure TFlowExtractor.GetObsTimes(FlowList: TFlowList; ObsTimes: TRealList);
var
  TimeIndex: Integer;
  ObsGroup: TCustomFlowGroup;
  Obs: TObservation;
  SP: TStressPeriod;
  GroupIndex: Integer;
begin
  for GroupIndex := 0 to FlowList.Count - 1 do
  begin
    ObsGroup := FlowList[GroupIndex];
    for TimeIndex := 0 to ObsGroup.ObservationTimes.Count - 1 do
    begin
      Obs := ObsGroup.ObservationTimes[TimeIndex];
      SP := FStressPeriods[Obs.ReferenceStressPeriod - 1];
      ObsTimes.AddUnique(SP.StartTime + Obs.TimeOffset);
      Obs.Time := SP.StartTime + Obs.TimeOffset;
    end;
  end;
end;

procedure TFlowExtractor.SortFlowObs;
var
  AList: TFlowList;
  index: Integer;
  ObsPosition: Integer;
  ObsGroup: TCustomFlowGroup;
begin
  for index := 0 to FFlowObs.FlowObs.Count - 1 do
  begin
    ObsGroup := FFlowObs.FlowObs[index];
    ObsPosition := FFlowLists.IndexOfLabel(ObsGroup.ObsLabel);
    if ObsPosition < 0 then
    begin
      AList := TFlowList.Create;
      FFlowLists.Add(AList);
    end
    else
    begin
      AList := FFlowLists[ObsPosition];
    end;
    AList.Add(ObsGroup);
  end;
end;

procedure TFlowExtractor.AnalyzeTimeSteps;
var
  StepIndex: Integer;
  StepLength: Extended;
  TotalTime: Double;
  TimeStep: TTimeStep;
  StressPeriodIndex: Integer;
  PerTime: Double;
  SP: TStressPeriod;
begin
  TotalTime := 0;
  for StressPeriodIndex := 0 to FStressPeriods.Count - 1 do
  begin
    SP := FStressPeriods[StressPeriodIndex];
    SP.StartTime := TotalTime;
    PerTime := 0;
    if (SP.TSMULT = 1) or (SP.NSTP = 1) then
    begin
      for StepIndex := 1 to SP.NSTP do
      begin
        PerTime := StepIndex / SP.NSTP * SP.PERLEN;
        TimeStep := TTimeStep.Create;
        FTimeSteps.Add(TimeStep);
        TimeStep.StressPeriod := StressPeriodIndex + 1;
        TimeStep.Step := StepIndex;
        TimeStep.PerTime := PerTime;
        TimeStep.TotalTime := TotalTime + PerTime;
        TimeStep.StressPeriodType := SP.StressPeriodType;
        FTimes.Add(TimeStep.TotalTime);
      end;
    end
    else
    begin
      StepLength := SP.PERLEN * (SP.TSMULT - 1)
        / (Power(SP.TSMULT, SP.NSTP) - 1);
      for StepIndex := 1 to SP.NSTP do
      begin
        PerTime := PerTime + StepLength;
        StepLength := StepLength * SP.TSMULT;
        TimeStep := TTimeStep.Create;
        FTimeSteps.Add(TimeStep);
        TimeStep.StressPeriod := StressPeriodIndex + 1;
        TimeStep.Step := StepIndex;
        TimeStep.PerTime := PerTime;
        TimeStep.TotalTime := TotalTime + PerTime;
        TimeStep.StressPeriodType := SP.StressPeriodType;
        FTimes.Add(TimeStep.TotalTime);
      end;
    end;
    TotalTime := TotalTime + SP.PERLEN;
    SP.TotalTime := TotalTime;
  end;
end;

procedure TFlowExtractor.EvaluteSimulatedValues;
var
  FlowGroupIndex: Integer;
  FlowList: TFlowList;
  ObsTimes: TRealList;
  PriorEarlierIndex: Integer;
  TimeIndex: Integer;
  ClosestIndex: Integer;
  EarlierIndex: Integer;
  EarlierStep: TTimeStep;
  LaterStep: TTimeStep;
  ErrorMessage: string;
  // @name is accessed by layer, row, column, order
  EarlierValues: array of array of array of array of double;
  // @name is accessed by layer, row, column, order
  LaterValues: array of array of array of array of double;
  // @name is accessed by row, column, order
  EarlierSingleLayerValues: array of array of array of double;
  // @name is accessed by row, column, order
  LaterSingleLayerValues: array of array of array of double;
  CellIndex: Integer;
  ACell: TStructCell;
  L: Integer;
  R: Integer;
  C: Integer;
  CurrentLength: Integer;
  GroupIndex: Integer;
  AGroup: TStructuredFlowGroup;
  ObsCell: FlowObs.TStructuredCell;
  EarlierValue: double;
  LaterValue: Double;
  AnObs: TObservation;
  Value: Extended;
begin
  for FlowGroupIndex := 0 to FFlowLists.Count - 1 do
  begin
    FlowList := FFlowLists[FlowGroupIndex];
    ObsTimes := TRealList.Create;
    try
      ObsTimes.Sorted := True;
      GetObsTimes(FlowList, ObsTimes);

      PriorEarlierIndex := -2;
      for TimeIndex := 0 to ObsTimes.Count - 1 do
      begin
        ClosestIndex := FTimes.IndexOfClosest(ObsTimes[TimeIndex]);
        Assert(ClosestIndex >= 0);
        if FTimes[ClosestIndex] > ObsTimes[TimeIndex] then
        begin
          EarlierIndex := ClosestIndex-1;
        end
        else
        begin
          EarlierIndex := ClosestIndex;
        end;
        if EarlierIndex <> PriorEarlierIndex then
        begin
          LaterStep := nil;
          EarlierStep := nil;
          PriorEarlierIndex := EarlierIndex;
          EarlierStep := FTimeSteps[EarlierIndex];
          FCBC_Reader.FillCellList(FlowList.ObsLabel,
            EarlierStep.StressPeriod, EarlierStep.Step, FEarlierList,
            ErrorMessage);
          if ErrorMessage <> '' then
          begin
            raise EExtractError.Create(ErrorMessage);
          end;
          SetLength(EarlierValues, NLAY, NROW, NCOL, 0);
          SetLength(EarlierSingleLayerValues, NROW, NCOL, 0);
          for CellIndex := 0 to FEarlierList.Count - 1 do
          begin
            ACell := FEarlierList[CellIndex];
            try
              L := ACell.Layer -1;
              R := ACell.Row-1;
              C := ACell.Column-1;

              CurrentLength := Length(EarlierValues[L, R, C]);
              SetLength(EarlierValues[L, R, C], CurrentLength+1);
              EarlierValues[L, R, C, CurrentLength] := ACell.Value;

              CurrentLength := Length(EarlierSingleLayerValues[R, C]);
              SetLength(EarlierSingleLayerValues[R, C], CurrentLength+1);
              EarlierSingleLayerValues[R, C, CurrentLength] := ACell.Value;

            except
              raise EExtractError.Create(Format(
                'Error reading value for the cell at (%0:d, %1:d, %2:d) at Stress Period %4:d, Time Step %5:d.',
                [ACell.Layer, ACell.Row, ACell.Column, EarlierStep.StressPeriod,
                EarlierStep.Step]));
            end;
          end;

          if EarlierIndex+1 < FTimeSteps.Count then
          begin
            LaterStep := FTimeSteps[EarlierIndex+1];
            FCBC_Reader.FillCellList(FlowList.ObsLabel,
              LaterStep.StressPeriod, LaterStep.Step, FLaterList,
              ErrorMessage);
            if ErrorMessage <> '' then
            begin
              raise EExtractError.Create(ErrorMessage);
            end;
            SetLength(LaterValues, NLAY, NROW, NCOL, 0);
            SetLength(LaterSingleLayerValues, NROW, NCOL, 0);
            for CellIndex := 0 to FLaterList.Count - 1 do
            begin
              ACell := FLaterList[CellIndex];
              try
                L := ACell.Layer -1;
                R := ACell.Row-1;
                C := ACell.Column-1;

                CurrentLength := Length(LaterValues[L, R, C]);
                SetLength(LaterValues[L, R, C], CurrentLength+1);
                LaterValues[L, R, C, CurrentLength] := ACell.Value;

                CurrentLength := Length(LaterSingleLayerValues[R, C]);
                SetLength(LaterSingleLayerValues[R, C], CurrentLength+1);
                LaterSingleLayerValues[R, C, CurrentLength] := ACell.Value;
              except
                raise EExtractError.Create(Format(
                  'Error reading value for the cell at (%0:d, %1:d, %2:d) at Stress Period %4:d, Time Step %5:d.',
                  [ACell.Layer, ACell.Row, ACell.Column, LaterStep.StressPeriod,
                  LaterStep.Step]));
              end;
            end;
          end
          else
          begin
            LaterStep := nil;
            LaterValues := nil;
            LaterSingleLayerValues := nil;
          end;
        end;

        for GroupIndex := 0 to FlowList.Count - 1 do
        begin
          AGroup := FlowList[GroupIndex] as TStructuredFlowGroup;
          While (AGroup.FTimeIndex < AGroup.ObservationTimes.Count)
            and (AGroup.ObservationTimes[AGroup.FTimeIndex].Time
            < ObsTimes[TimeIndex]) do
          begin
            Inc(AGroup.FTimeIndex);
          end;
          if (AGroup.FTimeIndex < AGroup.ObservationTimes.Count)
            and (AGroup.ObservationTimes[AGroup.FTimeIndex].Time
            = ObsTimes[TimeIndex]) then
          begin
            AnObs := AGroup.ObservationTimes[AGroup.FTimeIndex];
            AnObs.SimulatedValue := 0;
            for CellIndex := 0 to AGroup.Cells.Count - 1 do
            begin
              ObsCell := AGroup.Cells[CellIndex];

              if ObsCell.Layer = -1 then
              begin
                EarlierValue := EarlierSingleLayerValues[ObsCell.Row-1,
                  ObsCell.Column-1, ObsCell.Order-1];
              end
              else
              begin
                EarlierValue := EarlierValues[ObsCell.Layer-1, ObsCell.Row-1,
                  ObsCell.Column-1, ObsCell.Order-1];
              end;
              if LaterStep <> nil then
              begin
                if ObsCell.Layer = -1 then
                begin
                  LaterValue := LaterSingleLayerValues[ObsCell.Row-1,
                    ObsCell.Column-1, ObsCell.Order-1];
                end
                else
                begin
                  LaterValue := LaterValues[ObsCell.Layer-1, ObsCell.Row-1,
                    ObsCell.Column-1, ObsCell.Order-1];
                end;

                if LaterStep.StressPeriodType = sptTransient then
                begin
                  Value := (AnObs.Time - EarlierStep.TotalTime)
                    / (LaterStep.TotalTime - EarlierStep.TotalTime)
                    * (LaterValue-EarlierValue)
                    + EarlierValue;
                end
                else
                begin
                  Value := LaterValue;
                end;
              end
              else
              begin
                Value := EarlierValue;
              end;
              AnObs.SimulatedValue :=
                AnObs.SimulatedValue + Value * ObsCell.Factor;
            end;
          end;
        end;

      end;


    finally
      ObsTimes.Free;
    end;
  end;
end;

procedure TFlowExtractor.ReadFiles(const NameFile: string);
var
  DisReader: TDisFileReader;
  FlowReader: TFlowReader;
  GroupIndex: Integer;
  ObsGroup: TCustomFlowGroup;
  TimeIndex: Integer;
  Obs: TObservation;
begin
  ReadNameFile(NameFile);

  FFlowObs := TFlowObservations.Create;
  FStressPeriods := TStressPeriods.Create;
  FTimeSteps := TTimeStepList.Create;
  FFlowLists := TFlowLists.Create;
  FTimes := TRealList.Create;
  FEarlierList := TStructuredCellList.Create;
  FLaterList := TStructuredCellList.Create;
  try
    FlowReader := TFlowReader.Create;
    try
      FlowReader.ReadFlowObsInputFile(FInputFileName, FFlowObs, FListingFile);
    finally
      FlowReader.Free;
    end;
    SortFlowObs;

    DisReader := TDisFileReader.Create;
    try
      DisReader.ReadDiscretization(FDisFileName, NLAY, NROW, NCOL,
        FStressPeriods, FListingFile);
    finally
      DisReader.Free;
    end;
    AnalyzeTimeSteps;


    FCBC_Reader := TCBC_Reader.Create(FFlowFileName, FFlowObs.Precision);
    try
      EvaluteSimulatedValues;
    finally
      FCBC_Reader.Free;
    end;

    for GroupIndex := 0 to FFlowObs.FlowObs.Count - 1 do
    begin
      ObsGroup := FFlowObs.FlowObs[GroupIndex];
      for TimeIndex := 0 to ObsGroup.ObservationTimes.Count - 1 do
      begin
        Obs := ObsGroup.ObservationTimes[TimeIndex];
        FOutputFile.Write(Obs.SimulatedValue);
        FOutputFile.Write('  ');
        FOutputFile.Write(Obs.ObservedValue);
        FOutputFile.Write('  ');
        FOutputFile.WriteLine(Obs.OBSNAM);
      end;
    end;

  finally
    FLaterList.Free;
    FEarlierList.Free;
    FTimes.Free;
    FFlowLists.Free;
    FTimeSteps.Free;
    FStressPeriods.Free;
    FFlowObs.Free;
  end;
end;

procedure TFlowExtractor.ReadNameFile(const NameFile: string);
var
  index: Integer;
  slNameFile: TStringList;
begin
  slNameFile := TStringList.Create;
  try
    slNameFile.LoadFromFile(NameFile);
    FListingFileName := '';
    FDisFileName := '';
    FInputFileName := '';
    FOutputFileName := '';
    FFlowFileName := '';
    for index := 0 to slNameFile.Count - 1 do
    begin
      if (slNameFile[index] = '') or (slNameFile[index][1] = '#') then
      begin
        Continue;
      end;
      FLineSplitter.DelimitedText := slNameFile[index];
      if FLineSplitter.Count < 2 then
      begin
        raise ENameFileError.Create(Format('Invalid line in Name file: %s',
          [slNameFile[index]]));
      end;
      if UpperCase(FLineSplitter[0]) = 'LIST' then
      begin
        if FListingFileName <> '' then
        begin
          raise ENameFileError.Create(Format(
            'Invalid duplicate LIST file in Name file: %s',
            [slNameFile[index]]));
        end
        else
        begin
          FListingFileName := FLineSplitter[1];
        end;
      end
      else if UpperCase(FLineSplitter[0]) = 'DIS' then
      begin
        if FDisFileName <> '' then
        begin
          raise ENameFileError.Create(Format(
            'Invalid duplicate DIS file in Name file: %s',
            [slNameFile[index]]));
        end
        else
        begin
          FDisFileName := FLineSplitter[1];
        end;
      end
      else if UpperCase(FLineSplitter[0]) = 'MAIN' then
      begin
        if FInputFileName <> '' then
        begin
          raise ENameFileError.Create(Format(
            'Invalid duplicate MAIN file in Name file: %s',
            [slNameFile[index]]));
        end
        else
        begin
          FInputFileName := FLineSplitter[1];
        end;
      end
      else if UpperCase(FLineSplitter[0]) = 'OUT' then
      begin
        if FOutputFileName <> '' then
        begin
          raise ENameFileError.Create(Format(
            'Invalid duplicate OUT file in Name file: %s',
            [slNameFile[index]]));
        end
        else
        begin
          FOutputFileName := FLineSplitter[1];
        end;
      end
      else if UpperCase(FLineSplitter[0]) = 'CBC' then
      begin
        if FFlowFileName <> '' then
        begin
          raise ENameFileError.Create(Format(
            'Invalid duplicate CBC file in Name file: %s',
            [slNameFile[index]]));
        end
        else
        begin
          FFlowFileName := FLineSplitter[1];
        end;
      end;
    end;
    //    ListingFile := nil;
    if FListingFileName = '' then
    begin
      raise ENameFileError.Create('No LIST file specified in Name file');
    end
    else
    begin
      FListingFile := TFile.CreateText(FListingFileName);
      FListingFile.WriteLine(
        'General Flow Observations Version ' + FileVersion);
      FListingFile.WriteLine('');
      FListingFile.WriteLine(slNameFile.Text);
      FListingFile.WriteLine('');
    end;
  finally
    slNameFile.Free;
  end;
  if FDisFileName = '' then
  begin
    raise ENameFileError.Create(
      'No DIS file specified in Name file');
  end;
  if FInputFileName = '' then
  begin
    raise ENameFileError.Create(
      'No MAIN file specified in Name file');
  end;
  if FOutputFileName = '' then
  begin
    raise ENameFileError.Create(
      'No OUT file specified in Name file');
  end;
  if FFlowFileName = '' then
  begin
    raise ENameFileError.Create(
      'No CBC file specified in Name file');
  end;
  if not TFile.Exists(FInputFileName) then
  begin
    raise ENameFileError.Create(Format(
      'The MAIN input file does not exist: %s', [FInputFileName]));
  end
  else
  begin
    FListingFile.WriteLine('Main input file = ' + FInputFileName);
  end;
  if not TFile.Exists(FDisFileName) then
  begin
    raise ENameFileError.Create(Format(
      'The DIS file does not exist: %s', [FDisFileName]));
  end
  else
  begin
    FListingFile.WriteLine('Discretization file = ' + FDisFileName);
  end;
  if not TFile.Exists(FFlowFileName) then
  begin
    raise ENameFileError.Create(Format(
      'The CBC file does not exist: %s', [FFlowFileName]));
  end
  else
  begin
    FListingFile.WriteLine('Cell-by-cell flow file = ' + FFlowFileName);
  end;
  FListingFile.WriteLine('Output file = ' + FOutputFileName);
  FListingFile.WriteLine('');

  FOutputFile := TFile.CreateText(FOutputFileName);

end;

end.
