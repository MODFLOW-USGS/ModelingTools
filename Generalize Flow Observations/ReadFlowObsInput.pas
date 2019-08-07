unit ReadFlowObsInput;

interface

uses
  System.SysUtils, System.Generics.Collections, System.IOUtils, System.Classes,
  FlowObs;

type
  TPrecision = (pSingle, pDouble);
  TModelType = (mtStructured, mtUnstructured);

  TFlowObservations = class(TObject)
  private
    FPrecision: TPrecision;
    FModelType: TModelType;
    FFlowObs: TObjectList<TCustomFlowGroup>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Precision: TPrecision read FPrecision write FPrecision;
    property ModelType: TModelType read FModelType write FModelType;
    property FlowObs: TObjectList<TCustomFlowGroup> read FFlowObs;
  end;

  TFlowReader = class(TObject)
  private
    FInputFile: TStreamReader;
    FLineSplitter: TStringList;
    FFlowObservations: TFlowObservations;
    FOutputFile: TStreamWriter;
    procedure ReadComments(var ALine: string);
    procedure ReadDataSet2(const ALine: string);
    procedure ReadDataSet3(var CellGroup: TCustomFlowGroup);
    procedure ReadDataSet4(CellGroup: TCustomFlowGroup);
  public
    procedure ReadFlowObsInputFile(const InputFileName: string;
      FlowObs: TFlowObservations;
      OutputFile: TStreamWriter);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TFlowReader.Create;
begin
  FLineSplitter := TStringList.Create;
  FLineSplitter.Delimiter := ' ';
  FLineSplitter.QuoteChar := '"';
end;

destructor TFlowReader.Destroy;
begin
  FLineSplitter.Free;
  inherited;
end;

procedure TFlowReader.ReadDataSet4(CellGroup: TCustomFlowGroup);
var
  TimeOffset: Double;
  TimeIndex: Integer;
  Obs: TObservation;
  StressPeriod: Integer;
  ObservedValue: double;
  OutputLine: TStringBuilder;
  Blanks: TCharArray;
  Index: Integer;
begin
  FOutputFile.WriteLine('Observation    Reference      Time           Observed');
  FOutputFile.WriteLine('Name           Stress Period  Offset         Value');
  FOutputFile.WriteLine('-----------    -------------  ------         ----------');

  SetLength(Blanks, 15);
  for Index := 0 to Length(Blanks) - 1 do
  begin
    Blanks[Index] := ' ';
  end;

  OutputLine := TStringBuilder.Create;
  try
    for TimeIndex := 0 to CellGroup.ObservationTimes.Capacity - 1 do
    begin

      FLineSplitter.DelimitedText := FInputFile.ReadLine;
      if FLineSplitter.Count < 4 then
      begin
        raise EFlowReader.Create(Format(
          'Error reading data set 5 %0:d for observation group %1:d. Too few input variables. Too few variables on line',
          [TimeIndex + 1, FFlowObservations.FlowObs.Count]));
      end;

      Obs := TObservation.Create;
      CellGroup.ObservationTimes.Add(Obs);
      Obs.OBSNAM := Copy(FLineSplitter[0], 1,12);
      OutputLine.Clear;
      OutputLine.Append(Obs.OBSNAM);
      OutputLine.Append(Blanks, 0, 15-OutputLine.Length);

      if TryStrToInt(FLineSplitter[1], StressPeriod) then
      begin
        Obs.ReferenceStressPeriod := StressPeriod;
        OutputLine.Append(StressPeriod);
        OutputLine.Append(Blanks, 0, 30-OutputLine.Length)
      end
      else
      begin
        raise EFlowReader.Create(Format(
          'Error reading reference stress period %0:d for observation group %1:d. Not an integer: %2:s.',
          [TimeIndex + 1, FFlowObservations.FlowObs.Count, FLineSplitter[1]]));
      end;

      if TryStrToFloat(FLineSplitter[2], TimeOffset) then
      begin
        Obs.TimeOffset := TimeOffset;
        OutputLine.Append(TimeOffset);
        OutputLine.Append(Blanks, 0, 45-OutputLine.Length)
      end
      else
      begin
        raise EFlowReader.Create(Format(
          'Error reading observation time offset %0:d for observation group %1:d. Not a number: %2:s.',
          [TimeIndex + 1, FFlowObservations.FlowObs.Count, FLineSplitter[2]]));
      end;

      if TryStrToFloat(FLineSplitter[3], ObservedValue) then
      begin
        Obs.ObservedValue := ObservedValue;
        OutputLine.Append(ObservedValue);
        OutputLine.Append(Blanks, 0, 60-OutputLine.Length)
      end
      else
      begin
        raise EFlowReader.Create(Format(
          'Error reading observed value %0:d for observation group %1:d. Not a number: %2:s.',
          [TimeIndex + 1, FFlowObservations.FlowObs.Count, FLineSplitter[3]]));
      end;
      FOutputFile.WriteLine(OutputLine.ToString);
    end;
  finally
    OutputLine.Free;
  end;
  FOutputFile.WriteLine('');
end;

procedure TFlowReader.ReadDataSet3(var CellGroup: TCustomFlowGroup);
var
  TimeCount: Integer;
  CellCount: Integer;
begin
  FOutputFile.WriteLine(Format('  Flow Observation Group %d',
    [FFlowObservations.FlowObs.Count + 1]));
  FOutputFile.WriteLine       ('------------------------------');
  // Read data set 4
  FLineSplitter.DelimitedText := FInputFile.ReadLine;
  if FLineSplitter.Count < 3 then
  begin
    raise EFlowReader.Create(Format(
      'Error reading data set 4 for observation group %d. Too few input variables.',
      [FFlowObservations.FlowObs.Count + 1]));
  end;
  CellGroup := nil;
  case FFlowObservations.FModelType of
    mtStructured:
      begin
        CellGroup := TStructuredFlowGroup.Create;
      end;
    mtUnstructured:
      begin
        CellGroup := TUnstructuredFlowGroup.Create;
      end;
  else
    Assert(False);
  end;
  FFlowObservations.FlowObs.Add(CellGroup);
  if TryStrToInt(FLineSplitter[0], TimeCount) then
  begin
    CellGroup.ObservationTimes.Capacity := TimeCount;
    FOutputFile.WriteLine(Format('  Number of observation times %d',
      [TimeCount]));
  end
  else
  begin
    raise EFlowReader.Create(Format(
      'Error reading number of observation times in data set 4 for observation group %d.',
      [FFlowObservations.FlowObs.Count]));
  end;
  if TryStrToInt(FLineSplitter[1], CellCount) then
  begin
    CellGroup.CellCapacity := CellCount;
    FOutputFile.WriteLine(Format('  Number of observation cells %d',
      [CellCount]));
  end
  else
  begin
    raise EFlowReader.Create(Format(
      'Error reading number of cells in data set 4 for observation group %d.',
      [FFlowObservations.FlowObs.Count]));
  end;

  CellGroup.ObsLabel := FLineSplitter[2];
  FOutputFile.WriteLine(Format('  The Flow Type is "%s"',
    [CellGroup.ObsLabel]));

  CellGroup.UniformFactor := (FLineSplitter.Count >= 4)
    and (UpperCase(FLineSplitter[3]) = 'UNIFORM');
  if CellGroup.UniformFactor then
  begin
    FOutputFile.WriteLine('  The FACTOR for all the cells is 1');
  end
  else
  begin
    FOutputFile.WriteLine(
      '  The FACTOR for each the cells is specified individually');
  end;
  FOutputFile.WriteLine('');
end;

procedure TFlowReader.ReadDataSet2(const ALine: string);
var
  NumberOfCellGroups: Integer;
  Precision: string;
  ModelType: string;
begin
  // Read data set 2.
  FLineSplitter.DelimitedText := ALine;
  if FLineSplitter.Count < 3 then
  begin
    raise EFlowReader.Create(
      'Error reading data set 2. Too few input variables.');
  end;
  if TryStrToInt(FLineSplitter[0], NumberOfCellGroups) then
  begin
    FFlowObservations.FlowObs.Capacity := NumberOfCellGroups;
    FOutputFile.WriteLine(Format('  Number of Cell Groups = %d',
      [NumberOfCellGroups]));
  end
  else
  begin
    raise EFlowReader.Create(
      'Could not read number of cell groups in Data Set 2.');
  end;
  Precision := UpperCase(FLineSplitter[1]);
  if Precision = 'SINGLE' then
  begin
    FFlowObservations.Precision := pSingle;
    FOutputFile.WriteLine('  Precision = SINGLE');
  end
  else if Precision = 'DOUBLE' then
  begin
    FFlowObservations.Precision := pDouble;
    FOutputFile.WriteLine('  Precision = DOUBLE');
  end
  else
  begin
    raise EFlowReader.Create('Could not read budget precision in Data Set 2.');
  end;

  ModelType := UpperCase(FLineSplitter[2]);
  if ModelType = 'STRUCTURED' then
  begin
    FFlowObservations.ModelType := mtStructured;
    FOutputFile.WriteLine('  Model Type = STRUCTURED');
  end
  else if ModelType = 'UNSTRUCTURED' then
  begin
    FFlowObservations.ModelType := mtUnstructured;
    FOutputFile.WriteLine('  Model Type = UNSTRUCTURED');
  end
  else
  begin
    raise EFlowReader.Create('Could budget precision in Data Set 2.');
  end;
  FOutputFile.WriteLine('');
end;

procedure TFlowReader.ReadComments(var ALine: string);
begin
  while not FInputFile.EndOfStream do
  begin
    ALine := FInputFile.ReadLine;
    if (ALine = '') or (ALine[1] = '#') then
    begin
      FOutputFile.WriteLine(ALine);
      // Write to output file
    end
    else
    begin
      Exit;
    end;
  end;
end;

procedure TFlowReader.ReadFlowObsInputFile(const InputFileName: string;
  FlowObs: TFlowObservations;
  OutputFile: TStreamWriter);
var
  ALine: string;
  CellGroupIndex: Integer;
  CellGroup: TCustomFlowGroup;
begin
  Assert(OutputFile <> nil);
  FOutputFile := OutputFile;
  FFlowObservations := FlowObs;
  FFlowObservations.Clear;
  FInputFile := nil;
  try
    try
      FInputFile := TFile.OpenText(InputFileName);
      // Read data set 1.
      ReadComments(ALine);
      ReadDataSet2(ALine);

      for CellGroupIndex := 0 to FFlowObservations.FlowObs.Capacity - 1 do
      begin
        ReadDataSet3(CellGroup);
        ReadDataSet4(CellGroup);
        // read data set 5
        CellGroup.ReadCells(FInputFile, FLineSplitter, FOutputFile);
      end;

    finally
      FInputFile.Free;
    end;
  except on E: Exception do
    begin
      FOutputFile.WriteLine(E.Message);
      raise;
    end;
  end;
end;

{ TFlowObservations }

procedure TFlowObservations.Clear;
begin
  FFlowObs.Clear;
end;

constructor TFlowObservations.Create;
begin
  FFlowObs := TObjectList<TCustomFlowGroup>.Create;
end;

destructor TFlowObservations.Destroy;
begin
  FFlowObs.Free;
  inherited;
end;

end.
